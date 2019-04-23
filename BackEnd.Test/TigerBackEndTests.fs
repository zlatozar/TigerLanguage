module Tests

open NUnit.Framework

open FsUnit.Xunit
open Xunit

open Canon

// _____________________________________________________________________________
//                                              Correct Tiger language examples

let mergeTig = "../../../../testcases/merge.tig"

let displayExps fileName =
    let fragments = Tiger.FrontEnd.transFromFile mergeTig

    let display stm =
        match stm with
        | Frame.PROC proc -> printf "%A\n\n" (linearize proc.body)
        | _               -> printf "\n\n"

    List.iter display fragments

[<Fact>]
let ``Tiger program should be linearised without errors`` () =
    displayExps mergeTig