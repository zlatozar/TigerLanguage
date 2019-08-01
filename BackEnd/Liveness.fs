module Liveness

open GraphRep
open Flow

type IGraph =
    { graph: Graph;
      tnode: Temp.Temp -> Node;
      gtemp: Node -> Temp.Temp;
      moves: (Node * Node) list }

// temps index * list of temps
type LiveSet = Temp.Table<unit> * Temp.Temp list

// Map of Node:LiveSet (coresponing live-out Temps)
type LiveMap = Graph.Table<LiveSet>

//_____________________________________________________________________________
//                                                     Set operations on lists

// Add in sorted set
let rec add e set =
    match set with
    | []       -> [e]
    | hd :: tl -> if e = hd then hd :: tl
                  else
                      if e < hd then e :: hd :: tl
                      else hd :: add e tl

let rec union setX setY =
    match setX, setY with
    | [], _       -> setY
    | _, []       -> setX
    | hd :: tl, _ -> add hd (union tl setY)

let rec diff setX setY =
    match setX, setY with
    |  [], _      -> []
    |  _, []      -> setX
    | hd :: tl, _ -> if List.contains hd setY then diff tl setY
                     else add hd (diff tl setY)

// NOTE p.225: Create redundant representation of temps that are live-out.
//             Map to check memeber ship in the future steps and list for enumerating.
let liveness ({control=control; def=def; uses=uses; isMove=_} :FlowGraph) :LiveMap =
    let fnodes = Graph.nodes control
    let emptyTable =
        List.fold (fun tab fnode -> Graph.Table.add tab fnode []) Graph.Table.empty fnodes

    let rec loop inTable outTable =
        let (in', out') =
            // use 'foldBack' - liveness calculation following REVERSE control-flow edges
            List.foldBack (fun fnode (inT, outT) ->
                                            // use[n], def[n]
                                            let uses' = Graph.Table.lookup uses fnode
                                            let defs' = Graph.Table.lookup def fnode
                                            // out[n]
                                            let liveOut' = Graph.Table.lookup outT fnode

                                            // in[n] = union of use[n] and (out[n] - def[n])
                                            let liveIn = union uses' (diff liveOut' defs')

                                            // out[n] = union of the live-in sets of all successors of n
                                            let liveOut =
                                                List.fold (fun liveout succ ->
                                                                     union liveout (Graph.Table.lookup inT succ)
                                                          ) liveOut' (Graph.succ fnode)

                                            let outT' = Graph.Table.add outT fnode liveOut
                                            let inT' = Graph.Table.add inT fnode liveIn

                                            (inT', outT')
                          ) fnodes (inTable, outTable)

        // When all liveness information (outs and ins) not changed, the calculation is ended
        if List.forall
            (fun fnode ->
                Graph.Table.lookup inTable fnode = Graph.Table.lookup in' fnode  &&
                Graph.Table.lookup outTable fnode = Graph.Table.lookup out' fnode) fnodes
        then (in', out')
        else loop in' out'

    // start calculating liveness following forward control-flow edges
    let (_, out) = loop emptyTable emptyTable

    // It is interesting to know what is live at the exit of each node
    List.fold (fun liveMap fnode ->
                                    let liveList = Graph.Table.lookup out fnode
                                    let liveSet =
                                        List.fold (fun tab t -> Temp.Table.enter tab t ()) Temp.Table.empty liveList

                                    Graph.Table.add liveMap fnode (liveSet, liveList)) Graph.Table.empty fnodes

// debugging
let showLiveMap (liveMap :LiveMap) =
    printfn "\n["
    Graph.Table.iter (fun n m -> printf "\t%A -> %A\n" n m) liveMap
    printfn "]\n"

let interferenceGraph (flowGraph :FlowGraph) :IGraph * (Node -> Temp.Temp list) =

    let liveMap = liveness flowGraph
    let igraph = Graph.newGraph()

    let {control=control; def=def; uses=uses; isMove=isMove} = flowGraph

    let addINode (tnode, gtemp) temp =
        if Temp.Table.contain tnode temp
            then (tnode, gtemp)
            else
                let inode = Graph.newNode igraph
                let tnode' = Temp.Table.enter tnode temp inode
                let gtemp' = Graph.ITable.add gtemp inode temp
                (tnode', gtemp')

    // Connect nodes that interfere
    let addIEdge (n1: Node) (n2: Node) =
        if not (n1 = n2) && not (List.exists (Graph.eq n1) (Graph.adj n2))
            then Graph.mkEdge n1 n2
            else ()

    // Builds IGraph as creating inodes and fill tables
    let rec loop tnode gtemp moves fnodes =
        match fnodes with
        | []              -> // The graph itself and the instruments to search in graph database
                             { graph=igraph;
                               tnode=(fun t     -> Temp.Table.lookup tnode t);
                               gtemp=(fun inode -> Graph.ITable.lookup gtemp inode);
                               moves=List.rev moves }

        | fnode :: restNodes ->
                                // Find out kind of temps in flow graph
                                let uses' = Graph.Table.lookup uses fnode
                                let def' = Graph.Table.lookup def fnode
                                let isMove' = Graph.Table.lookup isMove fnode

                                let (liveKey, liveList) = Graph.Table.lookup liveMap fnode

                                // Note that we pass liveList. In this way we catch all temps up to now.
                                let (tnode', gtemp') = List.fold addINode (tnode, gtemp) (uses' @ def' @ liveList)

                                // Tip: The head of def[] and use[] are temps defined/used in current node.

                                let moves' = if isMove'
                                                 then
                                                     let a = Temp.Table.lookup tnode' (List.head def')
                                                     let c = Temp.Table.lookup tnode' (List.head uses')
                                                     (a, c) :: moves
                                                 else moves

                                let adj = if isMove' && Temp.Table.contain liveKey (List.head uses')
                                              then List.filter (fun t -> t <> List.head uses') liveList // don't add self-edge
                                              else liveList

                                List.iter (fun d -> let inode = Temp.Table.lookup tnode' d
                                                    let iadj = List.map (fun t -> Temp.Table.lookup tnode' t) adj
                                                    List.iter (addIEdge inode) iadj) def'

                                // continue with the rest and accumulate moves
                                loop tnode' gtemp' moves' restNodes

    let flowNodes = Graph.nodes control
    let iGraph = loop Temp.Table.empty Graph.ITable.empty [] flowNodes

    // function thar returns live-out temps in a given node
    let liveOut fnode = snd (Graph.Table.lookup liveMap fnode)
    (iGraph, liveOut)

let showIGraph ({graph=graph; tnode=_; gtemp=gtemp; moves=_} :IGraph) =
    let say = printfn "%s"
    say "\n";
    List.iter  (fun n -> say (sprintf "%s:\t%s: %i\n"
                               (Frame.tempName (gtemp n))
                               (String.concat ", " (List.map (fun gn -> Frame.tempName (gtemp gn)) (Graph.adj n)))
                               (List.length (Graph.adj n)))) (Graph.nodes graph)