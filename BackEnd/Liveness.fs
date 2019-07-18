module Liveness

open GraphRep
open Flow

type IGraph =
    { graph: Graph;
      tnode: Temp.Temp -> Node;
      gtemp: Node -> Temp.Temp;
      moves: (Node * Node) list }

type LiveSet = Temp.Table<unit> * Temp.Temp list

// Node and coresponing Temps
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
  |  [], _ -> []
  |  _, [] -> setX
  | hd :: tl, _ -> if List.contains hd setY then diff tl setY
                   else add hd (diff tl setY)

let liveness ({control=control; def=def; uses=uses; isMove=_} :FlowGraph) :LiveMap =
    let fnodes = Graph.nodes control
    let emptyTable =
        List.fold (fun tab fnode -> Graph.Table.add tab fnode []) Graph.Table.empty fnodes

    let rec loop inTable outTable =
        let (in', out') =
            List.foldBack (fun fnode (inT, outT) ->
                                            let uses' = Graph.Table.lookup uses fnode
                                            let def' = Graph.Table.lookup def fnode

                                            let liveOut = Graph.Table.lookup outT fnode

                                            let liveIn' = union uses' (diff liveOut def')
                                            let liveOut' =
                                                List.fold (fun liveout succ ->
                                                      union liveout (Graph.Table.lookup inT succ)) liveOut (Graph.succ fnode)

                                            let inT' = Graph.Table.add inT fnode liveIn'
                                            let outT' = Graph.Table.add outT fnode liveOut'

                                            (inT', outT')
                          ) fnodes (inTable, outTable)

        if List.forall
            (fun fnode ->
                Graph.Table.lookup inTable fnode = Graph.Table.lookup in' fnode  &&
                Graph.Table.lookup outTable fnode = Graph.Table.lookup out' fnode) fnodes
        then (in', out')
        else loop in' out'

    let (_, out) = loop emptyTable emptyTable

    List.fold (fun livemap fnode ->
                                    let livelist = Graph.Table.lookup out fnode
                                    let liveset =
                                        List.fold (fun tab t -> Temp.Table.enter tab t ()) Temp.Table.empty livelist

                                    Graph.Table.add livemap fnode (liveset, livelist)) Graph.Table.empty fnodes

// debugging
let showLiveMap (liveMap :LiveMap) =
    printfn "\n["
    Graph.Table.iter (fun n m -> printf "\t%A -> %A\n" n m) liveMap
    printfn "]\n"