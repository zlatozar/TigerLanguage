module Tokens

(*
   A "scaffold" structure for debugging lexer.
   Using it start and end positions of tokens could be checked.
*)

type LineNum = int
type Token = string

// _____________________________________________________________________________
//                                                                       Tokens

let ARRAY(i, _) = sprintf "ARRAY  %i" i
let BREAK(i, _) = sprintf "BREAK  %i" i
let DO(i, _) = sprintf "DO  %i" i
let ELSE(i, _) = sprintf "ELSE  %i" i
let END(i, _) = sprintf "END  %i" i
let FOR(i, _) = sprintf "FOR  %i" i
let FUNCTION(i, _) = sprintf "FUNCTION  %i" i
let IF(i, _) = sprintf "IF  %i" i
let IN(i, _) = sprintf "IN  %i" i
let LET(i, _) = sprintf "LET  %i" i
let NIL(i, _) = sprintf "NIL  %i" i
let OF(i, _) = sprintf "OF  %i" i
let THEN(i, _) = sprintf "THEN  %i" i
let TO(i, _) = sprintf "TO  %i" i
let TYPE(i, _) = sprintf "TYPE   %i" i
let VAR(i, _) = sprintf  "VAR   %i" i
let WHILE(i, _) = sprintf "WHILE  %i" i

let ASSIGN(i, _) = sprintf "ASSIGN  %i" i
let DOT(i, _) = sprintf "DOT  %i" i
let LPAREN(i, _) = sprintf "LPAREN  %i" i
let RPAREN(i, _) = sprintf "RPAREN  %i" i
let LBRACK(i, _) = sprintf "LBRACK  %i" i
let RBRACK(i, _) = sprintf "RBRACK  %i" i
let LBRACE(i, _) = sprintf "LBRACE  %i" i
let RBRACE(i, _) = sprintf "RBRACE  %i" i
let SEMICOLON(i, _) = sprintf "SEMICOLON  %i" i
let COLON(i, _) = sprintf "COLON  %i" i
let COMMA(i, _) = sprintf "COMMA  %i" i

let AND(i, _) = sprintf "AND  %i" i
let OR(i, _) = sprintf "OR  %i" i
let TIMES(i, _) = sprintf "TIMES  %i" i
let DIVIDE(i, _) = sprintf "DIVIDE  %i" i
let PLUS(i, _) = sprintf "PLUS  %i" i
let MINUS(i, _) = sprintf "MINUS  %i" i
let EQ(i, _) = sprintf "EQ  %i" i
let NEQ(i, _) = sprintf "NEQ  %i" i
let GT(i, _) = sprintf "GT  %i" i
let GE(i, _) = sprintf "GE  %i" i
let LT(i, _) = sprintf "LT  %i" i
let LE(i, _) = sprintf "LE  %i" i

let STRING(s, i, _) = sprintf "STRING(%s)  %i " s i
let INT(c, i, j) = sprintf "INT(%i)  %i, %i" c i j
let ID(s, i, _) = sprintf "ID(%s)  %i" s i
let EOF(i, _) = sprintf "EOF  %i" i
