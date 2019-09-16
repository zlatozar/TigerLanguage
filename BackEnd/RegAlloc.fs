module RegAlloc

type Allocation = Temp.Table<Frame.Register>

type Replace = {
    replacement: Temp.Table<Temp.Temp>;
    instrs: Assem.Instr list;
    temps: Temp.Temp list
}

let rewrite_program spilled frame instrs =

    let mem =
        List.fold
            (fun t mem -> Temp.Table.enter t mem (Frame.allocFrameLocal frame true))
                Temp.Table.empty spilled

    let replace temps instr =
        List.foldBack
            (fun t replace ->
                if List.contains t spilled then
                    let t' = Temp.newTemp()

                    { replacement = Temp.Table.enter replace.replacement t' t;
                      instrs = instr t t' @ replace.instrs;
                      temps = t' :: replace.temps }
                else
                    replace
            )  temps {replacement = Temp.Table.empty; instrs = []; temps = []}

    let replace_src temps =
        replace temps
            (fun t t' ->
                Codegen.codegen frame (Tree.MOVE (Tree.TEMP t', Frame.exp (Temp.Table.lookup mem t) (Tree.TEMP Frame.FP))))

    let replace_dst temps =
        replace temps
            (fun t t' ->
                Codegen.codegen frame (Tree.MOVE (Frame.exp (Temp.Table.lookup mem t) (Tree.TEMP Frame.FP), Tree.TEMP t')))

    let replace_temps temps tab =
        List.map
            (fun t -> match Temp.Table.look tab t with
                      | Some x -> x
                      | None   -> t) temps

    let rec loop = function
        | [] -> ([], [])

        | Assem.OPER {assem=assem; dst=dst; src=src; jump=jump} :: instrs ->
                        let src_replace = replace_src src
                        let dst_replace = replace_dst dst

                        let instr = Assem.OPER {
                            assem=assem;
                            src = replace_temps src src_replace.replacement;
                            dst = replace_temps dst dst_replace.replacement;
                            jump=jump
                        }

                        let (temps, instrs') = loop instrs

                        (src_replace.temps @ dst_replace.temps @ temps,
                            src_replace.instrs @ [instr] @ dst_replace.instrs @ instrs')

        | Assem.MOVE {assem=assem; dst=dst; src=src} :: instrs ->
                        let src_replace = replace_src [src]
                        let dst_replace = replace_dst [dst]

                        let instr = Assem.MOVE {
                            assem=assem;
                            src = List.head (replace_temps [src] src_replace.replacement);
                            dst = List.head (replace_temps [dst] dst_replace.replacement)
                        }

                        let (temps, instrs') = loop instrs

                        (src_replace.temps @ dst_replace.temps @ temps,
                            src_replace.instrs @ [instr] @ dst_replace.instrs @ instrs')

        | (Assem.LABEL _ as instr) :: instrs -> let (temps, instrs') = loop instrs
                                                (temps, instr :: instrs')

    // Start rewriting
    loop instrs

let eliminate_moves allocation instrs =
    List.filter
        (function
             | Assem.MOVE {assem=_; dst=dst; src=src}
                   when Temp.Table.lookup allocation src = Temp.Table.lookup allocation dst -> false
             | _                                                                            -> true ) instrs

open Flow
open Liveness
open GraphRep

let alloc (instrs: Assem.Instr list) (frame: Frame.Frame) :Assem.Instr list * Temp.Table<Frame.Register> =

    let rec loop instrs rewrite_temps =

        let ({FlowGraph.control=_; def=def; uses=uses; isMove=_} as flowgraph, flownodes) = MakeGraph.instrs2graph instrs
        let ({IGraph.graph=_; tnode=_; gtemp=gtemp; moves=_} as igraph, _) = interferenceGraph flowgraph

        let spill_cost =
            let usedefs =
                List.fold
                    (fun tab fnode -> let tab' = List.fold
                                                     (fun tab t ->
                                                          let (nuse, ndef) = match Temp.Table.look tab t with
                                                                             | Some (x, y) -> (x, y)
                                                                             | None        -> (0, 0)
                                                          Temp.Table.enter tab t (nuse + 1, ndef)
                                                     ) tab (Graph.Table.lookup uses fnode)

                                      List.fold
                                          (fun tab t -> let (nuse, ndef) = match Temp.Table.look tab t with
                                                                           | Some (x, y) -> (x, y)
                                                                           | None        -> (0, 0)
                                                        Temp.Table.enter tab t (nuse, ndef + 1)
                                          ) tab' (Graph.Table.lookup def fnode)

                    ) Temp.Table.empty flownodes

            // return spill cost function
            (fun inode -> let t = gtemp inode
                          let (uses, def) = Temp.Table.lookup usedefs t

                          if List.contains t rewrite_temps then
                              System.Int32.MaxValue
                          else
                              uses + def)

        let (allocation, spilled) = Color.color igraph spill_cost Frame.tempMap Frame.registers

        if List.isEmpty spilled then
            let instrs' = eliminate_moves allocation instrs
            (instrs, allocation)
        else
            let (temps, instrs') = rewrite_program spilled frame instrs
            loop instrs' temps

    loop instrs []
