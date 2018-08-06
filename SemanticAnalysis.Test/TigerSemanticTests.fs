module TigerSemanticTests

open System
open FsUnit.Xunit
open Xunit

open Env
open Types
open Tiger.Semantic

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
//                                                                  Tests Cases

// [<Fact>]
// let ``All correct Tiger programs should pass without errors`` () =
//     List.iter (fun test -> transFromFile test |> printf "%A\n") correctExamples
//     |> should not' (be Empty)

// _____________________________________________________________________________
//                                                     Tests during development

[<Fact>]
let ``Get actual type and assignment`` () =
    transFromString """
let
    type a = int
    type b = string

    type result = b

    var first:int := 10
    var second:int := 20
    var third:result := "String testing"
in
    first := second + first
end
"""

[<Fact>]
let ``Simple and correct if/then/else`` () =
    transFromString "if (10 > 20) then 30 else 40"

[<Fact>]
let ``Correct 'for' cycle`` () =
    transFromString """
let
    var a:= 0
in
    for i:=0 to 100 do (a:=a+1;break;())
end
"""

[<Fact>]
let ``Correct 'while' cycle`` () =
    transFromString """
let
    var a:= 0
in
    while a<10 do (a:=a+1;break;())
end
"""

[<Fact>]
let ``Cyclic types definition`` () =
    transFromString """
let
   type a = b
   type b = d
   type c = a
   type d = a
in
end
"""

[<Fact>]
let ``Correct array types`` () =
    transFromString """
let
    type myint = int
    type  arrtype = array of myint

    var arr1:arrtype := arrtype [10] of 0
in
    arr1
end
"""

[<Fact>]
let ``Correct array expressions`` () =
    transFromString """
let
    type a = array of int
    type b = a

    var arr1:a := b [10] of 0
in
    arr1[2]
end
"""

[<Fact>]
let ``Correct record definition`` () =
    transFromString """
let
    type rectype = {name:string, age:int}
    type emptyRectype = {}
in
end
"""

[<Fact>]
let ``Correct record expression`` () =
    transFromString """
let
    type  rectype = {name:string, age:int}
    var rec1:rectype := rectype {name="Nobody", age=1000}
in
    rec1
end
"""

[<Fact>]
let ``Corrrect field variable assignment`` () =
    transFromString """
let
    type  rectype = {name:string, age:int}
    var rec1:rectype := rectype {name="Nobody", age=1000}
in
    rec1.name := "Somebody";
    rec1
end
"""

[<Fact>]
let ``Corrrect function definition`` () =
    transFromString """
let
    function double(n: int): int = n * 2
in
    double(10)
end
"""

[<Fact>]
let ``Corrrect recursive function definition`` () =
    transFromString """
let
    function nfactor(n: int): int =
        if n = 0
          then 1
          else n * nfactor(n - 1)
in
    nfactor(10)
end
"""

[<Fact>]
let ``Corrrect mutual recursive function definition`` () =
    transFromString """
let
    function do_nothing1(a: int, b: string) :int=
              (do_nothing2(a + 1); 0)

    function do_nothing2(d: int):string =
              (do_nothing1(d, "str"); " ")
in
    do_nothing1(0, "str2")
end
"""
