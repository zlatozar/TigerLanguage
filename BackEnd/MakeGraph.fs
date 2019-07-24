module MakeGraph

open GraphRep
open Flow

let instrs2graph (instrs :Assem.Instr list) :FlowGraph * Node list =

    //_________________________________________________________________________
    //                                                        Helper functions

    // init new graph
    let empty = new ResizeArray<NodeRep>()

    let takeNext lst =
        match lst with
        | []   -> []
        | [a]  -> [a]
        | a::_ -> [a]

    // map label to Node
    let labelMap = List.fold (fun map instr -> match instr with
                                               | Assem.LABEL {assem=_; lab=lab} ->
                                                                           Store.enter (map, lab, (Graph.newNode empty))
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

    // Wind till the end and initialize the 'control' with an empty graph.
    // Then un-wind(backword) as apply the logic having previous node.
    let rec instrs2graph' (instrList: Assem.Instr list) :FlowGraph * Node list =
        match instrList with
        | []            -> ({control=empty; // init
                             def=Graph.Table.empty;
                             uses=Graph.Table.empty;
                             isMove=Graph.Table.empty}, [])

        | Assem.OPER {assem=_; src=src; dst=dst; jump=jump} :: instrs
                        -> let node = Graph.newNode empty

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           // follow label or take next node
                           let succ =
                               match jump with
                               | Some ns -> List.map labelNode ns
                               | None    -> takeNext nodes

                           mkSucc node succ

                           ({control=control;
                             def=Graph.Table.add def node (List.sort dst);
                             uses=Graph.Table.add uses node (List.sort src) ;
                             isMove=Graph.Table.add isMove node false}, node :: nodes)

        | Assem.LABEL {assem=_; lab=lab} :: instrs
                        -> let node = labelNode lab

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           mkSucc node (takeNext nodes)

                           ({control=control;
                             def=Graph.Table.add def node [];
                             uses=Graph.Table.add uses node [];
                             isMove=Graph.Table.add isMove node false}, node :: nodes)

        | Assem.MOVE {assem=_; src=src; dst=dst} :: instrs
                        -> let node = Graph.newNode empty

                           let ({control=control; def=def; uses=uses; isMove=isMove} :FlowGraph, nodes) = instrs2graph' instrs

                           mkSucc node (takeNext nodes)

                           ({control=control;
                             def=Graph.Table.add def node [dst];
                             uses=Graph.Table.add uses node [src];
                             isMove=Graph.Table.add isMove node true}, node :: nodes)

    instrs2graph' instrs
