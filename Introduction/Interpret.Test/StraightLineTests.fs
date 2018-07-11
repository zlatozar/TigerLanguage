module StraightLineTest

open System
open FsUnit.Xunit
open Xunit

open Absyn
open LinePar
open StraightLine

// _________________________________________________________________________
//                                                               Test lexer

let simple = AssignStmt ("a",OpExp (NumExp 5,Plus,NumExp 3))

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
let ``Statment counter: simple one`` () =
    stmtCounter simple |> should equal 1

[<Fact>]
let ``Statment counter: book example`` () =
    stmtCounter bookExample |> should equal 4

[<Fact>]
let ``Print arguments counter: simple one`` () =
    maxArgs simple |> should equal 0

[<Fact>]
let ``Print arguments counter: book example`` () =
    maxArgs bookExample |> should equal 2

[<Fact>]
let ``Lookup should succeed`` () =
    lookup [("a", 4); ("b", 5); ("c", 6)] "b" |> should equal 5

[<Fact>]
let ``Lookup should fail`` () =
    (fun () -> lookup [("a", 4); ("b", 5); ("c", 6)] "e" |> ignore)
        |> should throw typeof<System.Exception>

[<Fact>]
let ``Update`` () =
    update [("a", 5); ("b", 6)] "a" 10 |> should equal [("a", 10); ("b", 6)]

[<Fact>]
let ``Interpet the book example`` () =
    run "a := 5 + 3 ; b := ( print ( a , a - 1 ) , 10 * a ) ; print ( b )" |> should equal [("b", 80); ("a", 8)]
