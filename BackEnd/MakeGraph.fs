module MakeGraph

open GraphRep
open Flow

let instrs2graph (instrs :Assem.instr list) :FlowGraph * Node list =

    //_________________________________________________________________________
    //                                                        Helper functions

    // init new graph
    let graph = Graph.newGraph

    let takeNext lst =
        match lst with
        | []   -> []
        | [a]  -> [a]
        | a::_ -> [a]

    // map label to Node
    let labelMap = List.fold (fun map instr -> match instr with
                                               | Assem.LABEL {assem=_; lab=lab} ->
                                                                           Store.enter (map, lab, (Graph.newNode graph))
                                               | _                              -> map
                                                                 ) Store.empty instrs

    // find Node by given label
    let labelNode (lab: Temp.Label) =
        match (Store.lookup (labelMap, lab)) with
        | Some l -> l
        | None   -> failwithf "ERROR: Invalid label `%s`." (Store.name lab)

    // create directed graph as making edges
    let mkSucc node succ = List.iter (Graph.mkEdge node) succ

    //_________________________________________________________________________
    //                                                          Implementation

    // Recursivly Iterate over all assembly instructions type

    let rec instrs2graph' (instrList: Assem.instr list) :FlowGraph * Node list =
        match instrList with
        | []            -> ({control=graph;
                             def=Graph.Table.empty;
                             uses=Graph.Table.empty;
                             isMove=Graph.Table.empty}, [])

        | Assem.OPER {assem=_; src=src; dst=dst; jump=jump} :: instrs
                        -> let node = Graph.newNode graph

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           // follow label or take next node
                           let succ =
                               match jump with
                               | Some ns -> List.map labelNode ns
                               | None    -> List.take 1 nodes

                           mkSucc node succ

                           ({control=control;
                            def=Graph.Table.add def (List.sort dst) node;
                            uses=Graph.Table.add uses (List.sort src) node;
                            isMove=Graph.Table.add isMove false node}, node :: nodes)

        | Assem.LABEL {assem=_; lab=lab} :: instrs
                        -> let node = labelNode lab

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           mkSucc node (takeNext nodes)

                           ({control=control;
                             def=Graph.Table.add def [] node;
                             uses=Graph.Table.add uses [] node;
                             isMove=Graph.Table.add isMove false node}, node :: nodes)

        | Assem.MOVE {assem=_; src=src; dst=dst} :: instrs
                        -> let node = Graph.newNode graph

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           mkSucc node (takeNext nodes)

                           ({control=control;
                            def=Graph.Table.add def [dst] node;
                            uses=Graph.Table.add uses [src] node;
                            isMove=Graph.Table.add isMove true node}, node :: nodes)

    instrs2graph' instrs
