module Color

#nowarn "25"

open System.Collections.Generic

type Allocation = Temp.Table<Frame.Register>

type Dll<'a when 'a: equality> = DLList.NodeType<'a> option ref

type DllNode<'a when 'a: equality> = {
                        mutable node: DLList.NodeType<'a> option;
                        mutable dll: Dll<'a>
                      }

let equalRefs = LanguagePrimitives.PhysicalEquality

let createDll = fun() -> ref None

// Helper functions

let dll2list dll =
    match !dll with
    | Some lst -> DLList.toList lst
    | None     -> []

let moveNode setNode dll =
    let oldDll = setNode.dll

    let head = Option.get !oldDll
    let node = Option.get setNode.node
    let next = DLList.drop node

    setNode.dll <- dll

    // Remove from old dll
    if next = node then
        oldDll := None

    else if node = head then
        oldDll := Some next

    // Append to new dll
    match !dll with
    | None       -> dll := Some node
    | Some other -> DLList.splice (DLList.prev other) node

// Node representation
[<CustomEquality;CustomComparison>]
type CNode = {
               inode: GraphRep.Node;
               n: int;
               setNode: DllNode<CNode>
             }
     with
         override __.Equals n2 =
             match n2 with
             | :? CNode as n2 -> __.n = n2.n
             | _              -> false
         override __.GetHashCode() = hash __.n

         interface System.IComparable with
             member __.CompareTo nObj =
                 match nObj with
                 | :? CNode as n2 -> compare __.n n2.n
                 | _              -> failwith "ERROR: Cannot compare values of different types."

// 'MOVE' representation
[<CustomEquality;NoComparison>]
type Move = {
              src: CNode;
              dst: CNode;
              setNode: DllNode<Move>
            }
     with
         override __.Equals m2 =
             match m2 with
             | :? Move as m2 -> __.src = m2.src && __.dst = m2.dst
             | _             -> false
         override __.GetHashCode() = hash __.src

type CSet = Set<CNode>

module CSet =
    let empty = Set.empty<CNode>
    let add = Set.add<CNode>
    let diff = Set.difference<CNode>
    let ofList = Set.ofList<CNode>
    let union = Set.union<CNode>
    let fold = Set.fold
    let elements lst = Set.toList lst |> List.sort

type RSet = Set<Frame.Register>

module RSet =
    let empty = Set.empty<Frame.Register>
    let isEmpty = Set.isEmpty<Frame.Register>
    let ofList = Set.ofList<Frame.Register>
    let remove = Set.remove<Frame.Register>
    let choose = Set.minElement<Frame.Register>

//_____________________________________________________________________________
//                                                             Color algorithm

open GraphRep
open Liveness

let color ({graph=graph; tnode=_; gtemp=gtemp; moves=moves} :IGraph)
            (spill_cost: (Node -> int)) (allocation: Allocation)
            (registers: Frame.Register list) :Temp.Table<Frame.Register> * Temp.Temp list =

    let nextn = ref 0

    let add_node inode dll =
        let cnode = { inode=inode; n=(!nextn); setNode={node=None; dll=dll} }

        nextn := !nextn + 1;

        let dllNode = DLList.create cnode
        cnode.setNode.node <- Some dllNode

        match !dll with
        | None       -> dll := Some dllNode
        | Some other -> DLList.splice (DLList.prev other) dllNode

        cnode

    let add_move src dst dll =
        let move = { src=src; dst=dst; setNode={node=None; dll=dll} }

        let dllNode = DLList.create move
        move.setNode.node <- Some dllNode

        match !dll with
        | None       -> dll := Some dllNode
        | Some other -> DLList.splice (DLList.prev other) dllNode

        move

    // Start coloring algorithm

    let n = List.length (Graph.nodes graph)
    let k = List.length registers

    // Graph representation p. 241
    let adj_list = Array.create n CSet.empty
    let adj_set = Array2D.create n n false

//_____________________________________________________________________________
//                                           Node work-lists, sets, and stacks

    // machine registers, preassigned a color
    let precolored = createDll()
    // temporary registers, not precolored and not yet processed
    let initial = createDll()
    // list of low-degree non-move-related nodes
    let simplify_worklist = createDll()
    // low-degree move-related nodes
    let freeze_worklist = createDll()
    // high-degree nodes
    let spill_worklist = createDll()
    // nodes marked for spilling during this round; initially empty
    let spilled_nodes = createDll()

    // registers that have been coalesced; when u <- v; is coalesced, 'v'
    // is added to this set and 'u' put back on some work-list (or vice versa)
    let coalesced_nodes = createDll()

    // nodes successfully colored
    let colored_nodes = createDll()
    // stack containing temporaries removed from the graph
    let select_stack = createDll()

    (* Move sets *)

    // moves that have been coalesced
    let coalesced_moves = createDll()
    // moves whose source and target interfere
    let constrained_moves = createDll()
    // moves that will no longer be considered for coalescing
    let frozen_moves = createDll()
    // moves enabled for possible coalescing
    let worklist_moves = createDll()
    // moves not yet ready for coalescing
    let active_moves = createDll()

    (* Other data structurs *)

    // an array containing the current degree of each node.
    let degree = Array.create n 0
    // a mapping from a node to the list of moves it is associated with
    let move_list = Array.create n []
    // a move (u, v) has been coalesced, and 'u'; put in 'coalescedNodes', then alias(v) = u
    let alias = Array.create n None
    // the color chosen by the algorithm for a node; for precolored nodes this is
    // initialized to the given color.
    let color = Array.create n None

    let add_edge u v =
        if not adj_set.[u.n, v.n] && u <> v then

            adj_set.[u.n, v.n] <- true
            adj_set.[v.n, u.n] <- true

            if not (equalRefs u.setNode.dll precolored) then
                adj_list.[u.n] <- CSet.add v adj_list.[u.n]
                degree.[u.n]   <- degree.[u.n] + 1

            if not (equalRefs v.setNode.dll precolored) then
                adj_list.[v.n] <- CSet.add u adj_list.[v.n]
                degree.[v.n]   <- degree.[v.n] + 1

    // Constructs the interference graph (and bit matrix) using the results
    // of static liveness, and also initializes the 'worklistMoves' to contain
    // all the moves in the program.
    let build () =
        // (Node, CNode)
        let iNodeTab = List.fold (fun tab inode -> let temp = gtemp inode
                                                   let cnode = if Temp.Table.contain allocation temp then
                                                                   let cnode = add_node inode precolored
                                                                   color.[cnode.n] <- Some (Temp.Table.lookup allocation temp)
                                                                   cnode
                                                               else
                                                                    add_node inode initial
                                                   Graph.ITable.add tab inode cnode)
                                      Graph.ITable.empty (Graph.nodes graph)

        List.iter (fun inode -> let u = Graph.ITable.lookup iNodeTab inode
                                List.iter (fun adj -> add_edge u (Graph.ITable.lookup iNodeTab adj))
                                              (Graph.adj inode))
                      (Graph.nodes graph);

        List.iter (fun (u, v) -> let u = Graph.ITable.lookup iNodeTab u
                                 let v = Graph.ITable.lookup iNodeTab v
                                 let m = add_move u v worklist_moves

                                 move_list.[u.n] <- move_list.[u.n] @ [m]
                                 move_list.[v.n] <- move_list.[v.n] @ [m])
                      moves

    let node_moves n = List.filter
                           (fun m -> equalRefs m.setNode.dll active_moves || equalRefs m.setNode.dll worklist_moves) move_list.[n.n]

    let move_related n = node_moves n <> []

    let make_worklist() =
        List.iter (fun cnode -> if degree.[cnode.n] >= k then
                                    moveNode cnode.setNode spill_worklist
                                else
                                    if move_related cnode then
                                        moveNode cnode.setNode freeze_worklist
                                    else
                                        moveNode cnode.setNode simplify_worklist)
                       (dll2list initial)

    let adjacent cnode =
        CSet.elements
                      (CSet.diff adj_list.[cnode.n]
                          (CSet.union
                              (CSet.ofList (dll2list select_stack))
                                  (CSet.ofList (dll2list coalesced_nodes))))

    let enable_moves cnodes =
        List.iter (fun cnode -> List.iter (fun m ->
                                               if (equalRefs m.setNode.dll active_moves) then
                                                    moveNode m.setNode worklist_moves) (node_moves cnode))
                       cnodes

    let decrement_degree cnode =
        let d = degree.[cnode.n]
        degree.[cnode.n] <- degree.[cnode.n] - 1

        if d = k then
            enable_moves (cnode :: adjacent cnode);
            if move_related cnode then
                moveNode cnode.setNode freeze_worklist
            else
                moveNode cnode.setNode simplify_worklist

    let simplify () =
        List.iter (fun (cnode: CNode) -> moveNode cnode.setNode select_stack
                                         List.iter decrement_degree (adjacent cnode))
                      (dll2list simplify_worklist)

    let rec get_alias (cnode: CNode) =
        if (equalRefs cnode.setNode.dll coalesced_nodes) then
            get_alias (Option.get alias.[cnode.n])
        else
            cnode

    // Note: adj_set.[t.n, u.n] not adj_set.[t.n].[u.n]
    let ok (t: CNode) (u: CNode) = degree.[t.n] < k || (equalRefs t.setNode.dll precolored) || adj_set.[t.n, u.n]

    let conservative (nodes: CSet) =
        let d = CSet.fold (fun d n -> if degree.[n.n] >= k then d + 1 else d) 0 nodes
        d < k

    let add_worklist (u: CNode) =
        if not (equalRefs u.setNode.dll precolored) && not (move_related u) && degree.[u.n] < k
        then moveNode u.setNode simplify_worklist

    let combine (u: CNode) (v: CNode) =
        moveNode v.setNode coalesced_nodes
        alias.[v.n]     <- Some u
        move_list.[u.n] <- move_list.[u.n] @ move_list.[v.n]

        enable_moves [v]
        List.iter (fun t -> add_edge t u; decrement_degree t) (adjacent v)

        if degree.[u.n] >= k && (equalRefs u.setNode.dll freeze_worklist)
        then moveNode u.setNode spill_worklist


    let coalesce () =
        let ({src=src; dst=dst} as m) :: _ = dll2list worklist_moves

        let x = get_alias src
        let y = get_alias dst

        let (u, v) = if (equalRefs y.setNode.dll precolored) then (y, x) else (x, y)

        if u = v then
            moveNode m.setNode coalesced_moves
            add_worklist u

        else if (equalRefs v.setNode.dll precolored) || adj_set.[u.n, v.n] then
                 moveNode m.setNode constrained_moves
                 add_worklist u
                 add_worklist v

        else if (equalRefs u.setNode.dll precolored && List.forall (fun t -> ok t u) (adjacent v)) ||
                (not (equalRefs u.setNode.dll precolored) &&
                    conservative (CSet.union (CSet.ofList (adjacent u)) (CSet.ofList (adjacent v)))) then

                 moveNode m.setNode coalesced_moves
                 combine u v
                 add_worklist u

        else
            moveNode m.setNode active_moves

    let freeze_moves u =
        List.iter (fun move -> let v = if get_alias u = get_alias move.dst
                                       then get_alias move.src
                                       else get_alias move.dst

                               moveNode move.setNode frozen_moves

                               if not (move_related v) && degree.[v.n] < k
                               then moveNode v.setNode simplify_worklist)
                     (node_moves u)

    let freeze () =
        let u :: _ = dll2list freeze_worklist

        moveNode u.setNode simplify_worklist
        freeze_moves u

    let select_spill () =
        let cost cnode = spill_cost cnode.inode
        let m :: _ = List.sortWith (fun u v -> (cost u) - (cost v)) (dll2list spill_worklist)

        moveNode m.setNode simplify_worklist;
        freeze_moves m

    let assign_colors () =
        List.iter (fun cnode -> let ok_colors' =
                                    CSet.fold
                                        (fun ok_colors adj ->
                                             let adj' = get_alias adj

                                             if (equalRefs adj'.setNode.dll colored_nodes)
                                                || (equalRefs adj'.setNode.dll precolored)
                                             then
                                                 RSet.remove (Option.get color.[adj'.n]) ok_colors
                                             else
                                                 ok_colors

                                        ) (RSet.ofList registers) adj_list.[cnode.n]

                                if RSet.isEmpty ok_colors' then
                                    moveNode cnode.setNode spilled_nodes
                                else
                                    moveNode cnode.setNode colored_nodes;
                                    color.[cnode.n] <- Some (RSet.choose ok_colors')

                 ) (List.rev (dll2list select_stack))

        List.iter (fun cnode -> let alias = get_alias cnode
                                color.[cnode.n] <- color.[alias.n])
            (dll2list coalesced_nodes)

//_____________________________________________________________________________
//                                                                  Main Logic

    build()
    make_worklist()

    while not (List.isEmpty (dll2list simplify_worklist)) ||
          not (List.isEmpty (dll2list worklist_moves))    ||
          not (List.isEmpty (dll2list freeze_worklist))   ||
          not (List.isEmpty (dll2list spill_worklist))
        do
          if not (List.isEmpty (dll2list simplify_worklist)) then simplify()

          // guards functions from empty list so #nowarn "25" is OK
          else if not (List.isEmpty (dll2list worklist_moves) ) then coalesce()
          else if not (List.isEmpty (dll2list freeze_worklist)) then freeze()
          else if not (List.isEmpty (dll2list spill_worklist) ) then select_spill()
        done

    assign_colors()

    let allocation' =
      List.fold (fun alloc cnode -> let color = color.[cnode.n]
                                    match color with
                                    | Some c -> let t = gtemp cnode.inode
                                                Temp.Table.enter alloc t c
                                    | None   -> allocation

                ) allocation (dll2list colored_nodes @ dll2list coalesced_nodes)

    let spilled = List.map (fun cnode -> gtemp cnode.inode) (dll2list spilled_nodes)

    // Result
    (allocation', spilled)
