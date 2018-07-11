module ParserTest

open System
open FsUnit.Xunit
open Xunit

open Absyn
open LinePar
open FrontEnd

// _________________________________________________________________________
//                                                              Test parser

let bookExample =
  CompoundStmt
    (AssignStmt ("a",OpExp (NumExp 5,Plus,NumExp 3)),
     CompoundStmt
       (AssignStmt
          ("b",
           SeqExp
             (PrintStmt [IdExp "a"; OpExp (IdExp "a",Minus,NumExp 1)],
              OpExp (NumExp 10,Times,IdExp "a"))),PrintStmt [IdExp "b"]))

[<Fact>]
let ``Simple one`` () =
    LexParse.tryParse "a := 5 + 3" |> should equal (AssignStmt ("a",OpExp (NumExp 5,Plus,NumExp 3)))

[<Fact>]
let ``Book example`` () =
    LexParse.tryParse  "a := 5 + 3 ; b := ( print ( a , a - 1 ) , 10 * a ) ; print ( b )"
    |> should equal bookExample
