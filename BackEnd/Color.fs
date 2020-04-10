module Color

#nowarn "25"

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

    let addNode inode dll =
        let cnode = { inode=inode; n=(!nextn); setNode={node=None; dll=dll} }

        nextn := !nextn + 1;

        let dllNode = DLList.create cnode
        cnode.setNode.node <- Some dllNode

        match !dll with
        | None       -> dll := Some dllNode
        | Some other -> DLList.splice (DLList.prev other) dllNode

        cnode

    let addMove src dst dll =
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
    let adjList = Array.create n CSet.empty
    let adjSet = Array2D.create n n false

//_____________________________________________________________________________
//                                           Node work-lists, sets, and stacks

    // machine registers, preassigned a color
    let precolored = createDll()
    // temporary registers, not precolored and not yet processed
    let initial = createDll()
    // list of low-degree non-move-related nodes
    let simplifyWorklist = createDll()
    // low-degree move-related nodes
    let freezeWorklist = createDll()
    // high-degree nodes
    let spillWorklist = createDll()
    // nodes marked for spilling during this round; initially empty
    let spilledNodes = createDll()

    // registers that have been coalesced; when u <- v; is coalesced, 'v'
    // is added to this set and 'u' put back on some work-list (or vice versa)
    let coalescedNodes = createDll()

    // nodes successfully colored
    let coloredNodes = createDll()
    // stack containing temporaries removed from the graph
    let selectStack = createDll()

    (* Move sets *)

    // moves that have been coalesced
    let coalescedMoves = createDll()
    // moves whose source and target interfere
    let constrainedMoves = createDll()
    // moves that will no longer be considered for coalescing
    let frozenMoves = createDll()
    // moves enabled for possible coalescing
    let worklistMoves = createDll()
    // moves not yet ready for coalescing
    let activeMoves = createDll()

    (* Other data structurs *)

    // an array containing the current degree of each node.
    let degree = Array.create n 0
    // a mapping from a node to the list of moves it is associated with
    let moveList = Array.create n []
    // a move (u, v) has been coalesced, and 'u'; put in 'coalescedNodes', then alias(v) = u
    let alias = Array.create n None
    // the color chosen by the algorithm for a node; for precolored nodes this is
    // initialized to the given color.
    let color = Array.create n None

    let addEdge u v =
        if not adjSet.[u.n, v.n] && u <> v then

            adjSet.[u.n, v.n] <- true
            adjSet.[v.n, u.n] <- true

            if not (equalRefs u.setNode.dll precolored) then
                adjList.[u.n] <- CSet.add v adjList.[u.n]
                degree.[u.n]  <- degree.[u.n] + 1

            if not (equalRefs v.setNode.dll precolored) then
                adjList.[v.n] <- CSet.add u adjList.[v.n]
                degree.[v.n]  <- degree.[v.n] + 1

    // Constructs the interference graph (and bit matrix) using the results
    // of static liveness, and also initializes the 'worklistMoves' to contain
    // all the moves in the program.
    let build () =
        // (Node, CNode)
        let iNodeTab = List.fold (fun tab inode -> let temp = gtemp inode
                                                   let cnode = if Temp.Table.contain allocation temp then
                                                                   let cnode = addNode inode precolored
                                                                   color.[cnode.n] <- Some (Temp.Table.lookup allocation temp)
                                                                   cnode
                                                               else
                                                                    addNode inode initial
                                                   Graph.ITable.add tab inode cnode)
                                      Graph.ITable.empty (Graph.nodes graph)

        List.iter (fun inode -> let u = Graph.ITable.lookup iNodeTab inode
                                List.iter (fun adj -> addEdge u (Graph.ITable.lookup iNodeTab adj))
                                              (Graph.adj inode))
                      (Graph.nodes graph);

        List.iter (fun (u, v) -> let u = Graph.ITable.lookup iNodeTab u
                                 let v = Graph.ITable.lookup iNodeTab v
                                 let m = addMove u v worklistMoves

                                 moveList.[u.n] <- moveList.[u.n] @ [m]
                                 moveList.[v.n] <- moveList.[v.n] @ [m])
                      moves

    let nodeMoves n = List.filter
                           (fun m -> equalRefs m.setNode.dll activeMoves || equalRefs m.setNode.dll worklistMoves) moveList.[n.n]

    let moveRelated n = nodeMoves n <> []

    let makeWorklist() =
        List.iter (fun cnode -> if degree.[cnode.n] >= k then
                                    moveNode cnode.setNode spillWorklist
                                else
                                    if moveRelated cnode then
                                        moveNode cnode.setNode freezeWorklist
                                    else
                                        moveNode cnode.setNode simplifyWorklist)
                       (dll2list initial)

    let adjacent cnode =
        CSet.elements
                      (CSet.diff adjList.[cnode.n]
                          (CSet.union
                              (CSet.ofList (dll2list selectStack))
                                  (CSet.ofList (dll2list coalescedNodes))))

    let enableMoves cnodes =
        List.iter (fun cnode -> List.iter (fun m ->
                                               if (equalRefs m.setNode.dll activeMoves) then
                                                    moveNode m.setNode worklistMoves) (nodeMoves cnode))
                       cnodes

    let decrementDegree cnode =
        let d = degree.[cnode.n]
        degree.[cnode.n] <- degree.[cnode.n] - 1

        if d = k then
            enableMoves (cnode :: adjacent cnode);
            if moveRelated cnode then
                moveNode cnode.setNode freezeWorklist
            else
                moveNode cnode.setNode simplifyWorklist

    let simplify () =
        List.iter (fun (cnode: CNode) -> moveNode cnode.setNode selectStack
                                         List.iter decrementDegree (adjacent cnode))
                      (dll2list simplifyWorklist)

    let rec getAlias (cnode: CNode) =
        if (equalRefs cnode.setNode.dll coalescedNodes) then
            getAlias (Option.get alias.[cnode.n])
        else
            cnode

    // Note: adjSet.[t.n, u.n] not adjSet.[t.n].[u.n]
    let ok (t: CNode) (u: CNode) =
        degree.[t.n] < k || (equalRefs t.setNode.dll precolored) || adjSet.[t.n, u.n]

    let conservative (nodes: CSet) =
        let d = CSet.fold (fun d n -> if degree.[n.n] >= k then d + 1 else d) 0 nodes
        d < k

    let addWorklist (u: CNode) =
        if not (equalRefs u.setNode.dll precolored) && not (moveRelated u) && degree.[u.n] < k
        then moveNode u.setNode simplifyWorklist

    let combine (u: CNode) (v: CNode) =
        moveNode v.setNode coalescedNodes
        alias.[v.n]    <- Some u
        moveList.[u.n] <- moveList.[u.n] @ moveList.[v.n]

        enableMoves [v]
        List.iter (fun t -> addEdge t u; decrementDegree t) (adjacent v)

        if degree.[u.n] >= k && (equalRefs u.setNode.dll freezeWorklist)
        then moveNode u.setNode spillWorklist


    let coalesce () =
        let ({src=src; dst=dst} as m) :: _ = dll2list worklistMoves

        let x = getAlias src
        let y = getAlias dst

        let (u, v) = if (equalRefs y.setNode.dll precolored) then (y, x) else (x, y)

        if u = v then
            moveNode m.setNode coalescedMoves
            addWorklist u

        else if (equalRefs v.setNode.dll precolored) || adjSet.[u.n, v.n] then
                 moveNode m.setNode constrainedMoves
                 addWorklist u
                 addWorklist v

        else if (equalRefs u.setNode.dll precolored && List.forall (fun t -> ok t u) (adjacent v)) ||
                (not (equalRefs u.setNode.dll precolored) &&
                    conservative (CSet.union (CSet.ofList (adjacent u)) (CSet.ofList (adjacent v)))) then

                 moveNode m.setNode coalescedMoves
                 combine u v
                 addWorklist u

        else
            moveNode m.setNode activeMoves

    let freezeMoves u =
        List.iter (fun move -> let v = if getAlias u = getAlias move.dst
                                       then getAlias move.src
                                       else getAlias move.dst

                               moveNode move.setNode frozenMoves

                               if not (moveRelated v) && degree.[v.n] < k
                               then moveNode v.setNode simplifyWorklist)
                     (nodeMoves u)

    let freeze () =
        let u :: _ = dll2list freezeWorklist

        moveNode u.setNode simplifyWorklist
        freezeMoves u

    let selectSpill () =
        let cost cnode = spill_cost cnode.inode
        let m :: _ = List.sortWith (fun u v -> (cost u) - (cost v)) (dll2list spillWorklist)

        moveNode m.setNode simplifyWorklist;
        freezeMoves m

    let assignColors () =
        List.iter (fun cnode -> let okColors' =
                                    CSet.fold
                                        (fun okColors adj ->
                                             let adj' = getAlias adj

                                             if (equalRefs adj'.setNode.dll coloredNodes)
                                                || (equalRefs adj'.setNode.dll precolored)
                                             then
                                                 RSet.remove (Option.get color.[adj'.n]) okColors
                                             else
                                                 okColors

                                        ) (RSet.ofList registers) adjList.[cnode.n]

                                if RSet.isEmpty okColors' then
                                    moveNode cnode.setNode spilledNodes
                                else
                                    moveNode cnode.setNode coloredNodes;
                                    color.[cnode.n] <- Some (RSet.choose okColors')

                 ) (List.rev (dll2list selectStack))

        List.iter (fun cnode -> let alias = getAlias cnode
                                color.[cnode.n] <- color.[alias.n])
            (dll2list coalescedNodes)

//_____________________________________________________________________________
//                                                                  Main Logic

    build()
    makeWorklist()

    while not (List.isEmpty (dll2list simplifyWorklist)) ||
          not (List.isEmpty (dll2list worklistMoves))    ||
          not (List.isEmpty (dll2list freezeWorklist))   ||
          not (List.isEmpty (dll2list spillWorklist))
        do
          if not (List.isEmpty (dll2list simplifyWorklist)) then simplify()

          // guards functions from empty list so #nowarn "25" is OK
          else if not (List.isEmpty (dll2list worklistMoves) ) then coalesce()
          else if not (List.isEmpty (dll2list freezeWorklist)) then freeze()
          else if not (List.isEmpty (dll2list spillWorklist) ) then selectSpill()
        done

    assignColors()

    let allocation' =
      List.fold (fun alloc cnode -> let color = color.[cnode.n]
                                    match color with
                                    | Some c -> let t = gtemp cnode.inode
                                                Temp.Table.enter alloc t c
                                    | None   -> allocation

                ) allocation (dll2list coloredNodes @ dll2list coalescedNodes)

    let spilled = List.map (fun cnode -> gtemp cnode.inode) (dll2list spilledNodes)

    // Result
    (allocation', spilled)
