module MakeGraph

open GraphRep
open Flow

let instrs2graph (instrs :Assem.instr list) :Flow.FlowGraph * Node list =

    // Helper functions

    let control = Graph.newGraph

    let labelMap = List.fold (fun map instr -> match instr with
                                               | Assem.LABEL {assem=_; lab=lab} ->
                                                                           Store.enter (map, lab, (Graph.newNode control))
                                               | _                              -> map
                                                                 ) Store.empty instrs

    let labelNode lab = Option.get (Store.lookup (labelMap, lab))

    let mkSucc node succ = List.iter (Graph.mkEdge node) succ

    let rec instrs2graph' instrList :Flow.FlowGraph * Node list =
        match instrList with
        | [] -> ({control=control;
                  def=Graph.Table.empty;
                  uses=Graph.Table.empty;
                  isMove=Graph.Table.empty}, [])

        // FAKE
        | _  -> ({control=control;
                  def=Graph.Table.empty;
                  uses=Graph.Table.empty;
                  isMove=Graph.Table.empty}, [])

    instrs2graph' instrs