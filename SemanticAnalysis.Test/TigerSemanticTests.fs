module TigerSemanticTests

open NUnit.Framework

open FsUnit.Xunit
open Xunit

open Tiger.Semantic

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
                       "../../../../testcases/queens.tig";
                       "../../../../testcases/merge.tig"]    // most difficult

[<Fact>]
let ``All correct Tiger programs should pass without errors`` () =
    List.iter (transFromFile >> ignore) correctExamples
    !ErrorMsg.anyErrors |> should be False

// _____________________________________________________________________________
//                                          Tiger language examples with errors

let badExamples = ["../../../../testcases/bad/test09.tig";
                   "../../../../testcases/bad/test10.tig";
                   "../../../../testcases/bad/test11.tig";
                   "../../../../testcases/bad/test13.tig";
                   "../../../../testcases/bad/test14.tig";
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
                   "../../../../testcases/bad/test38.tig";
                   "../../../../testcases/bad/test39.tig";
                   "../../../../testcases/bad/test40.tig";
                   "../../../../testcases/bad/test43.tig";
                   "../../../../testcases/bad/test45.tig"]

[<Fact>]
let ``All incorrect Tiger programs should pass with errors`` () =
    List.iter (transFromFile >> ignore) badExamples
    !ErrorMsg.anyErrors |> should be True

// _____________________________________________________________________________
//                                                                   Additional

[<Fact>]
let ``Correct 'for' cycle`` () =
    transFromString """
let
    var a:= 0
in
    for i:=0 to 100 do (a:=a+1; break; ())
end
""" |> ignore
    !ErrorMsg.anyErrors |> should be False

[<Fact>]
let ``Correct 'while' cycle`` () =
    transFromString """
let
    var a:= 0
in
    while a<10 do (a:=a+1; break; ())
end
""" |> ignore
    !ErrorMsg.anyErrors |> should be False

[<Fact>]
let ``break should be only in cycles`` () =
    transFromString """
let
    var a := 0
in
    if a>0 then break
end
""" |> ignore
    !ErrorMsg.anyErrors |> should be True

[<Fact>]
let ``Should catch cyclic types definition`` () =
    transFromString """
let
   type a = b
   type b = d
   type c = a
   type d = a
in
end
""" |> ignore
    !ErrorMsg.anyErrors |> should be True

[<Fact>]
let ``or in test case`` () =
    transFromString """
let
    type any = { any: int }
    var buffer := getchar()

    function readint(any: any) :int =
        let
            function skipto() =
                while buffer = " "
                do buffer := getchar()

       in
           skipto();
           42
       end
in
end
""" |> ignore
    !ErrorMsg.anyErrors |> should be False
