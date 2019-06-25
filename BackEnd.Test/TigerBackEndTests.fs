module Tests

open NUnit.Framework

open FsUnit.Xunit
open Xunit

// Chapter 8
open Canon

// _____________________________________________________________________________
//                                              Correct Tiger language examples

let mergeTig = "../../../../testcases/merge.tig"

let displayExps fileName =
    let fragments = Tiger.FrontEnd.transFromFile fileName

    let display stm =
        match stm with
        | Frame.PROC proc -> printf "%A\n\n" (linearize proc.body)
        | _               -> printf "\n\n"

    List.iter display fragments

[<Fact>]
let ``Tiger program should be linearised without errors`` () =
    displayExps mergeTig

// Chapter 9

[<Fact>]
let ``Format assembly`` () =
    let t1 = Temp.newTemp()
    let t2 = Temp.newTemp()
    let t3 = Temp.newTemp() // 103

    let assemInstr = Assem.format Temp.makeString (Assem.OPER {Assem.assem = "add 'd0, 's0, 's1";
                                                   src = [t1; t2];
                                                   dst = [t3];
                                                   jump = None})
    // Generated assembler is aligned with tab to have room for labels
    assemInstr |> should equal "        add 103, 101, 102"