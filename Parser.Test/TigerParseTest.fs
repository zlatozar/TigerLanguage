module TigerParseTest

open System
open FsUnit.Xunit
open Xunit

open Tiger.Parser

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
                       "../../../../testcases/test14.tig";
                       "../../../../testcases/test27.tig";
                       "../../../../testcases/test30.tig";
                       "../../../../testcases/test37.tig";
                       "../../../../testcases/test38.tig";
                       "../../../../testcases/test39.tig";
                       "../../../../testcases/test41.tig";
                       "../../../../testcases/test42.tig";
                       "../../../../testcases/test43.tig";
                       "../../../../testcases/test44.tig";
                       "../../../../testcases/test46.tig";
                       "../../../../testcases/test47.tig";
                       "../../../../testcases/test48.tig";
                       "../../../../testcases/merge.tig";
                       "../../../../testcases/queens.tig"]

// _____________________________________________________________________________
//                                          Tiger language examples with errors

let badExamples = ["../../../../testcases/bad/test09.tig";
                   "../../../../testcases/bad/test10.tig";
                   "../../../../testcases/bad/test11.tig";
                   "../../../../testcases/bad/test13.tig";
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
                   "../../../../testcases/bad/test28.tig";
                   "../../../../testcases/bad/test29.tig";
                   "../../../../testcases/bad/test31.tig";
                   "../../../../testcases/bad/test32.tig";
                   "../../../../testcases/bad/test33.tig";
                   "../../../../testcases/bad/test34.tig";
                   "../../../../testcases/bad/test35.tig";
                   "../../../../testcases/bad/test36.tig";
                   "../../../../testcases/bad/test40.tig";
                   "../../../../testcases/bad/test45.tig";
                   "../../../../testcases/bad/test49.tig"]

// _____________________________________________________________________________
//                                                       Test during bug fixing

// Fix grammar

[<Fact>]
let ``Parsing record definition and creation`` () =
    fromString """ /* empty */
let
    type rectype = {name:string, age:int}
    var rec1:rectype := rectype {name="Nobody", age=1000}
in
    rec1.name := "Somebody";
    rec1
end
""" |> should not' (be Empty)

[<Fact>]
let ``Assign array`` () =
    fromString """ /* empty */
let
    type a = array of int
    type b = a
    var arr1:a := b[10] of 0
in
    arr1[2]
end
""" |> should not' (be Empty)

// Fix the order

[<Fact>]
let ``First book example`` () =
    fromString "(a := 5; a+1)" |> should not' (be Empty)

// Fix grouping of FunctionDec and TypeDec

[<Fact>]
let ``Second book example`` () =
    fromString """ /* empty */
let var a := 5
    function f() : int = g(a)
    function g(i: int) = f()
    in f()
end
""" |> should not' (be Empty)

[<Fact>]
let ``Third book example`` () =
    fromString """ /* empty */
let
    type tree = {key: int, children: treelist}
    type treelist = {head: tree, tail: treelist}
in
end
""" |> should not' (be Empty)


// _____________________________________________________________________________
//                                                                  Tests Cases

[<Fact>]
let ``All correct Tiger programs should pass without errors`` () =
    List.iter (fun test -> fromFile test |> printf "%A\n") correctExamples
    |> should not' (be Empty)
