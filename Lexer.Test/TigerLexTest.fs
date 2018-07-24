module Tests

open System
open FsUnit.Xunit
open Xunit

open Tiger.Lexer

// _____________________________________________________________________________
//                                                  All Tiger language examples

let testCases = ["../../../../testcases/merge.tig";
                 "../../../../testcases/queens.tig";
                 "../../../../testcases/test01.tig";
                 "../../../../testcases/test02.tig";
                 "../../../../testcases/test03.tig";
                 "../../../../testcases/test04.tig";
                 "../../../../testcases/test05.tig";
                 "../../../../testcases/test06.tig";
                 "../../../../testcases/test07.tig";
                 "../../../../testcases/test08.tig";
                 "../../../../testcases/bad/test09.tig";
                 "../../../../testcases/bad/test10.tig";
                 "../../../../testcases/bad/test11.tig";
                 "../../../../testcases/test12.tig";
                 "../../../../testcases/bad/test13.tig";
                 "../../../../testcases/test14.tig";
                 "../../../../testcases/bad/test15.tig";
                 "../../../../testcases/bad/test16.tig";
                 "../../../../testcases/bad/test17.tig";
                 "../../../../testcases/bad/test18.tig";
                 "../../../../testcases/bad/test19.tig";
                 "../../../../testcases/bad/test20.tig";
                 "../../../../testcases/bad/test21.tig";
                 "../../../../testcases/bad/test22.tig";
                 "../../../../testcases/bad/test23.tig";
                 "../../../../testcases/bad/test24.tig";
                 "../../../../testcases/bad/test25.tig";
                 "../../../../testcases/bad/test26.tig";
                 "../../../../testcases/test27.tig";
                 "../../../../testcases/bad/test28.tig";
                 "../../../../testcases/bad/test29.tig";
                 "../../../../testcases/test30.tig";
                 "../../../../testcases/bad/test31.tig";
                 "../../../../testcases/bad/test32.tig";
                 "../../../../testcases/bad/test33.tig";
                 "../../../../testcases/bad/test34.tig";
                 "../../../../testcases/bad/test35.tig";
                 "../../../../testcases/bad/test36.tig";
                 "../../../../testcases/test37.tig";
                 "../../../../testcases/test38.tig";
                 "../../../../testcases/test39.tig";
                 "../../../../testcases/bad/test40.tig";
                 "../../../../testcases/test41.tig";
                 "../../../../testcases/test42.tig";
                 "../../../../testcases/test43.tig";
                 "../../../../testcases/test44.tig";
                 "../../../../testcases/bad/test45.tig";
                 "../../../../testcases/test46.tig";
                 "../../../../testcases/test47.tig";
                 "../../../../testcases/test48.tig";
                 "../../../../testcases/bad/test49.tig"]

// _____________________________________________________________________________
//                                                                  Tests Cases

[<Fact>]
let ``All test cases should pass without errors`` () =
    List.iter (fun test -> fromFile test |> printf "%A\n") testCases
    |> should not' (be Empty)

// _____________________________________________________________________________
//                                                                   Edge Cases

[<Fact>]
let ``Continues lines`` () =
    fromString """ a := \
   \
5 + 3; """ |> should equal ["ID(a)  1"; "ASSIGN  3"; "INT(5)  0, 1"; "PLUS  2"; "INT(3)  4, 5";
   "SEMICOLON  5"; "EOF  7"]

[<Fact>]
let ``Correct nested comment`` () =
    fromString """ /* First /* Second */ */ """ |> should equal ["EOF  26"]

[<Fact>]
let ``Incorrect nested comment`` () =
    (fun () -> fromString """ /* First /* Second */ """ |> ignore)
       |> should throw typeof<System.Exception>

[<Fact>]
let ``Correct string`` () =
    fromString """ "First \042 \"inner\" 'inner again' \t" """
        |> should equal ["STRING(First * \"inner\" 'inner again' 	)  39 "; "EOF  41"]

[<Fact>]
let ``Lexical Error: Illegal escape sequence.`` () =
    (fun () -> fromString """ "First \042 \"inner\" 'inner again' \g" """ |> ignore)
       |> should throw typeof<System.Exception>

[<Fact>]
let ``Lexical Error: Unexpected char: '\'.`` () =
    // Fist string is closed, second starts with \"
    (fun () -> fromString """ "First \042 "inner\" 'inner again' \g" """ |> ignore)
       |> should throw typeof<System.Exception>

[<Fact>]
let ``Wierd error`` () =
    // Forgot to escape inner string. Now two strings with variable 'inner'
    fromString """ "First \042 "inner" 'inner again' \t" """
        |> should equal ["STRING(First * )  13 "; "ID(inner)  14"; "STRING( 'inner again' 	)  37 ";
   "EOF  39"]

[<Fact>]
let ``Special escape should work also`` () =
    fromString """ "\^v" """
        |> should equal ["STRING(\011)  5 "; "EOF  7"]
