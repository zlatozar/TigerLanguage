module LexerTest

open System
open FsUnit.Xunit
open Xunit

open LinePar
open FrontEnd

// _________________________________________________________________________
//                                                               Test lexer

[<Fact>]
let ``Simple one`` () =
    LexParse.tryLex "a := 5 + 3" |> should equal [ID "a"; ASSIGN; NUM 5; PLUS; NUM 3; EOF]

[<Fact>]
let ``Book example`` () =
    LexParse.tryLex "a := 5 + 3 ; b := ( print ( a , a - 1 ) , 10 * a ) ; print ( b )"
    |> should equal [ID "a"; ASSIGN; NUM 5; PLUS; NUM 3; SEMI; ID "b"; ASSIGN; LPAR; PRINT; LPAR;
                     ID "a"; COMMA; ID "a"; MINUS; NUM 1; RPAR; COMMA; NUM 10; TIMES; ID "a"; RPAR; SEMI; PRINT; LPAR; ID "b"; RPAR; EOF]
