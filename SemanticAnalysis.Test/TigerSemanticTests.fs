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
                       "../../../../testcases/test52.tig";
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
                   "../../../../testcases/bad/test45.tig";
                   "../../../../testcases/bad/test50.tig";
                   "../../../../testcases/bad/test51.tig"]

[<Fact>]
let ``All incorrect Tiger programs should pass with errors`` () =
    let doErrorTest fileName =
        transFromFile fileName |> ignore
        !ErrorMsg.anyErrors |> should be True
        ErrorMsg.reset

    List.iter doErrorTest badExamples

// _____________________________________________________________________________
//                                                                   Additional

// [<Fact>]
// let ``Correct 'for' cycle`` () =
//     transFromString """
// let
//     var a:= 0
// in
//     for i:=0 to 100 do (a:=a+1; break; ())
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be False

// [<Fact>]
// let ``Correct 'while' cycle`` () =
//     transFromString """
// let
//     var a:= 0
// in
//     while a<10 do (a:=a+1; break; ())
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be False

// [<Fact>]
// let ``break should be only in cycles`` () =
//     transFromString """
// let
//     var a := 0
// in
//     if a>0 then break
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be True

// [<Fact>]
// let ``Should catch cyclic types definition`` () =
//     transFromString """
// let
//    type a = b
//    type b = d
//    type c = a
//    type d = a
// in
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be True

// [<Fact>]
// let ``or in test case`` () =
//     transFromString """
// let
//     type any = { any: int }
//     var buffer := getchar()

//     function readint(any: any) :int =
//         let
//             function skipto() =
//                 while buffer = " " | buffer = "\n"
//                 do buffer := getchar()

//        in
//            skipto();
//            42
//        end
// in
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be False

// [<Fact>]
// let ``Deal with recursive records`` () =
//     transFromString """
// let
//     type list = { first: int, rest: list }

//     function merge(a: list, b: list) :list =
//         list{first=a.first, rest=merge(b.rest, b)}

// in
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be False

// [<Fact>]
// let ``Mutual recursive types`` () =
//     transFromString """
// let
//     type intlist = {hd: int, tl: intlist}

//     var lis:intlist := intlist { hd=0, tl=nil }
// in
// end
// """ |> ignore
//     !ErrorMsg.anyErrors |> should be False

// compare: t: `RECORD
//   ([(("first", 14), INT);
//     (("rest", 12), NAME (("list", 13),{contents = Some ...;}))],{contents = ();})` =
//  t2: `RECORD
//   ([(("first", 14), INT);
//     (("rest", 12), NAME (("list", 13),{contents = Some ...;}))],{contents = ();})`
