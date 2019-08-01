module Tests

#nowarn "25"

open NUnit.Framework

open FsUnit.Xunit
open Xunit

// NOTE: There is difference if we run suite or separate tests - Temps are with different numbers

// _____________________________________________________________________________
//                                                         Chapter 8 (Canon.fs)

open Canon

// correct example (testcases/test08.tig)
let tigExample = "if (10 > 20) then 30 else 40"

let displayExps =
    let fragments = Tiger.FrontEnd.transFromString tigExample

    let display frag =
        match frag with
        | Frame.PROC proc         -> Frame.prettyPrint (proc.body |> linearize |> basicBlocks |> traceSchedule)
        | Frame.STRING (label, s) -> printf "%s" (Frame.string (label, s))

    List.iter display fragments

[<Fact>]
let ``Tiger program should be linearised without errors`` () =
    displayExps

// _____________________________________________________________________________
//                                                                    Chapter 9

// MIPS code generation

let emitProc frag =
    match frag with
    | Frame.PROC {body=body; frame=frame} -> printf "%A\n\n"
                                                 (List.concat (List.map (Codegen.codegen frame) (body |> linearize |> basicBlocks |> traceSchedule)))
    | Frame.STRING (label, s)             -> printf "%s" (Frame.string (label, s))

[<Fact>]
let ``Assembly representation`` () =
    let fragmets = Tiger.FrontEnd.transFromString tigExample
    List.iter emitProc fragmets

// To assembly format

[<Fact>]
let ``Format assembly`` () =
    let t1 = Temp.newTemp() // 101 or 177 if run suite
    let t2 = Temp.newTemp()
    let t3 = Temp.newTemp()

    let expectedResult = if t1 = 101
                             then "add 103, 101, 102"
                             else "add 179, 177, 178"

    let assemInstr = Assem.format Temp.makeString (Assem.OPER {Assem.assem = "add 'd0, 's0, 's1";
                                                   src = [t1; t2];
                                                   dst = [t3];
                                                   jump = None})
    // Generated assembler is aligned with tab to have room for labels
    assemInstr.Trim() |> should equal expectedResult

// _____________________________________________________________________________
//                                                                   Chapter 10

// Graph representation and it's operations

open GraphRep

[<Fact>]
let ``Graph length`` () =
    let g = Graph.newGraph()

    Graph.newNode g |> ignore
    Graph.newNode g |> ignore

    g.Count |> should equal 2

[<Fact>]
let ``Make eadge and check successors and predcessors`` () =
    let g = Graph.newGraph()

    let n1 = Graph.newNode g
    let n2 = Graph.newNode g
    let n3 = Graph.newNode g

    Graph.mkEdge n1 n3
    Graph.mkEdge n2 n3

    (Graph.succ n1) |> should equal [n3]
    (Graph.succ n2) |> should equal [n3]
    (Graph.pred n3) |> should equal [n2; n1]

[<Fact>]
let ``Delete eadge and check successors and predcessors`` () =
    let g = Graph.newGraph()

    let n1 = Graph.newNode g
    let n2 = Graph.newNode g
    let n3 = Graph.newNode g
    Graph.mkEdge n1 n3
    Graph.mkEdge n2 n3

    Graph.rmEdge n2 n3

    (Graph.succ n2) |> should be Empty
    (Graph.pred n3) |> should equal [n1]

[<Fact>]
let ``Test graph adjacency`` () =
    let g = Graph.newGraph()

    let n1 = Graph.newNode g
    let n2 = Graph.newNode g
    let n3 = Graph.newNode g
    let n4 = Graph.newNode g

    Graph.mkEdge n1 n2
    Graph.mkEdge n2 n3
    Graph.mkEdge n3 n4
    Graph.mkEdge n4 n1

    (Graph.adj n1) |> should equal [n4; n2]
    (Graph.adj n2) |> should equal [n1; n3]
    (Graph.adj n3) |> should equal [n2; n4]
    (Graph.adj n4) |> should equal [n3; n1]

    Graph.rmEdge n2 n3

    (Graph.adj n2) |> should equal [n1]
    (Graph.adj n3) |> should equal [n4]

// Data flow problem

open Assem
open Flow

[<Fact>]
let ``Test basic dataflow graph`` () =
    let t1 = Temp.newTemp()
    let t2 = Temp.newTemp()

    // assembly program
    let instrs = [
        OPER {assem = "sw `s1, -4('s0)";
              src = [Frame.FP; t1];
              dst = [];
              jump = None};

        LABEL {assem = "l11";
               lab = Temp.namedLabel "l11"};

        MOVE {assem = "move 'd0, 's0";
              src = t1;
              dst = t2}]

    let ({control=control; def=def; uses=uses; isMove=isMove}, [n1; n2; n3]) = MakeGraph.instrs2graph instrs

    control.Count |> should equal 3

    // pred/succ
    (Graph.pred n1) |> should be Empty
    (Graph.succ n1) |> should equal [n2]
    (Graph.pred n2) |> should equal [n1]
    (Graph.succ n2) |> should equal [n3]
    (Graph.succ n3) |> should be Empty
    (Graph.pred n3) |> should equal [n2]

    // def
    (Graph.Table.lookup def n1) |> should be Empty
    (Graph.Table.lookup def n2) |> should be Empty
    (Graph.Table.lookup def n3) |> should equal [t2]

    let sortedResult = if t1 > Frame.FP
                           then [Frame.FP; t1]
                           else [t1; Frame.FP]
    // uses
    (Graph.Table.lookup uses n1) |> should equal sortedResult
    (Graph.Table.lookup uses n2) |> should be Empty
    (Graph.Table.lookup uses n3) |> should equal [t1]

    // isMove
    (Graph.Table.lookup isMove n1) |> should be False
    (Graph.Table.lookup isMove n2) |> should be False
    (Graph.Table.lookup isMove n3) |> should be True

[<Fact>]
let ``Test dataflow graph with labels`` () =
    let instrs = [
        OPER {assem = "b `j0";
              src = [];
              dst = [];
              jump = Some [Temp.namedLabel "l12"]};

        LABEL {assem = "l11";
               lab = Temp.namedLabel "l11"};

        LABEL {assem = "l12";
               lab = Temp.namedLabel "l12"}]

    let (_, [n1; n2; n3]) = MakeGraph.instrs2graph instrs

    // control graph edges
    (Graph.pred n1) |> should be Empty
    (Graph.succ n1) |> should equal [n3]
    (Graph.succ n2) |> should equal [n3]

// Liveness

let assertLiveSet (liveMap :Liveness.LiveMap) fnode temps =
    let (liveset, livelist) = Graph.Table.lookup liveMap fnode
    (List.map fst (Temp.Table.bindings liveset)) |> should equal temps
    livelist |> should equal temps

[<Fact>]
let ``Test liveness graph creation`` () =

    let t1 = Temp.newTemp()
    let t2 = Temp.newTemp()
    let t3 = Temp.newTemp()
    let t4 = Temp.newTemp()

    let instrs = [
        OPER {assem = "lw `d0, 1";
              src = [];
              dst = [t1];
              jump = None};

        OPER {assem = "lw `d0, 2";
              src = [];
              dst = [t2];
              jump = None};

        OPER {assem = "add 'd0, 's0, 's1";
              src = [t1; t2];
              dst = [t3];
              jump = None};

        OPER {assem = "addi 'd0, 's0, 5";
              src = [t3];
              dst = [t3];
              jump = None};

        OPER {assem = "jal 's0";
              src = [t4];
              dst = [];
              jump = None}]

    let (flowGraph, fnodes) = MakeGraph.instrs2graph instrs
    let liveMap = Liveness.liveness flowGraph

    List.iter2 (assertLiveSet liveMap) fnodes [[t1; t4]; [t1; t2; t4]; [t3; t4]; [t4]; []]

// Build interference graph

open Liveness

[<Fact>]
let ``Test interference graph creation with no MOVE instruction`` () =
    let t1 = Temp.newTemp()
    let t2 = Temp.newTemp()
    let t3 = Temp.newTemp()

    let instrs = [
        OPER {assem = "lw `d0, 1";
              src = [];
              dst = [t1];
              jump = None};

        OPER {assem = "lw `d0, 2";
              src = [];
              dst = [t2];
              jump = None};

        OPER {assem = "add 'd0, 's0, 's1";
              src = [t1; t2];
              dst = [t3];
              jump = None};

        OPER {assem = "addi 'd0, 's0, 5";
              src = [t3];
              dst = [t3];
              jump = None}]

    let (flowGraph, _) = MakeGraph.instrs2graph instrs

    let ({graph=graph; tnode=tnode; gtemp=gtemp; moves=_} as G, _) = interferenceGraph flowGraph
    (List.length (Graph.nodes graph)) |> should equal 3
    printfn "\nTemps are digints because the are not mapped to registers"
    showIGraph G

    List.iter (fun t -> gtemp (tnode t) |> should equal t) [t1; t2; t3]
    [tnode t2] |> should equal (Graph.adj (tnode t1))
    [tnode t1] |> should equal (Graph.adj (tnode t2))
    (Graph.adj (tnode t3)) |> should be Empty

[<Fact>]
let ``Test interference graph creation with a MOVE instruction`` () =
    let t1 = Temp.newTemp()
    let t2 = Temp.newTemp()
    let t3 = Temp.newTemp()
    let t4 = Temp.newTemp()

    let instrs = [
        OPER {assem = "lw `d0, 1";
              src = [];
              dst = [t4];
              jump = None};

        OPER {assem = "lw `d0, 1";
              src = [];
              dst = [t1];
              jump = None};

        MOVE {assem = "move 'd0, 's0";
              src = t1;
              dst = t2};

        OPER {assem = "add 'd0, 's0, 's1";
              src = [t1; t2];
              dst = [t3];
              jump = None};

        OPER {assem = "addi 'd0, 's0, 1";
              src = [t4];
              dst = [t4];
              jump = None};

        MOVE {assem = "move 'd0, 's0";
              src = t4;
              dst = t4}]

    let (flowGraph, _) = MakeGraph.instrs2graph instrs

    let ({graph=graph; tnode=tnode; gtemp=gtemp; moves=moves} as G, _) = interferenceGraph flowGraph
    (List.length (Graph.nodes graph)) |> should equal 4
    showIGraph G

    List.iter (fun t -> (gtemp (tnode t)) |> should equal t) [t1; t2; t3; t4]
    [tnode t4] |> should equal (Graph.adj (tnode t1))
    [tnode t4] |> should equal (Graph.adj (tnode t2))
    [tnode t4] |> should equal (Graph.adj (tnode t3))
    (List.map tnode [t3; t2; t1]) |> should equal (Graph.adj (tnode t4))

    (List.length moves) |> should equal 2

    List.iter2 (fun (dst, src) (dst', src') -> Graph.eq dst dst' |> should be True;
                                               Graph.eq src src' |> should be True
               ) moves [(tnode t2, tnode t1); (tnode t4, tnode t4)]