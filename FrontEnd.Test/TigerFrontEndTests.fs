module TigerFrontEndTests

open NUnit.Framework

open FsUnit.Xunit
open Xunit

open System.Text.RegularExpressions

open Tiger.FrontEnd

[<SetUp>]
let setUpBefore () = ErrorMsg.reset

// _____________________________________________________________________________
//                                              Correct Tiger language examples

let correctExamples = ["../../../../testcases/test01.tig";
                       "../../../../testcases/test02.tig";
                       "../../../../testcases/test03.tig";
                       "../../../../testcases/test04.tig";
                       "../../../../testcases/test05.tig";
                       "../../../../testcases/test06.tig";
                       "../../../../testcases/test07.tig";
                       "../../../../testcases/test08.tig";
                       "../../../../testcases/test12.tig";
                       "../../../../testcases/test27.tig";
                       "../../../../testcases/test30.tig";
                       "../../../../testcases/test37.tig";
                       "../../../../testcases/test41.tig";
                       "../../../../testcases/test42.tig";
                       "../../../../testcases/test44.tig";
                       "../../../../testcases/test46.tig";
                       "../../../../testcases/test47.tig";
                       "../../../../testcases/test48.tig";
                       "../../../../testcases/test52.tig";
                       "../../../../testcases/queens.tig";
                       "../../../../testcases/merge.tig"]  // most difficult

[<Fact>]
let ``All correct Tiger programs should pass without errors`` () =
    List.iter (transFromFile >> ignore) correctExamples
    !ErrorMsg.anyErrors |> should be False

[<Fact>]
let ``Simple RegEx test`` () =
    let pattern = Regex("sw 's0, ([0-9]+)\('s1\)")
    let matches = pattern.Match "sw 's0, 16('s1)"

    matches.Success |> should be True
    ((int) matches.Groups.[1].Value) |> should equal 16