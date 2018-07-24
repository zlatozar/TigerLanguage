module Tokens

(*
   A "scaffold" structure for debugging lexer.
   Using it start and end positions of tokens could be checked.
*)

type LineNum = int
type Token = string

// _____________________________________________________________________________
//                                                                       Tokens

let ARRAY(i, j) = sprintf "ARRAY  %i" i
let BREAK(i, j) = sprintf "BREAK  %i" i
let DO(i, j) = sprintf "DO  %i" i
let ELSE(i, j) = sprintf "ELSE  %i" i
let END(i, j) = sprintf "END  %i" i
let FOR(i, j) = sprintf "FOR  %i" i
let FUNCTION(i, j) = sprintf "FUNCTION  %i" i
let IF(i, j) = sprintf "IF  %i" i
let IN(i, j) = sprintf "IN  %i" i
let LET(i, j) = sprintf "LET  %i" i
let NIL(i, j) = sprintf "NIL  %i" i
let OF(i, j) = sprintf "OF  %i" i
let THEN(i, j) = sprintf "THEN  %i" i
let TO(i, j) = sprintf "TO  %i" i
let TYPE(i, j) = sprintf "TYPE   %i" i
let VAR(i, j) = sprintf  "VAR   %i" i
let WHILE(i, j) = sprintf "WHILE  %i" i

let ASSIGN(i, j) = sprintf "ASSIGN  %i" i
let DOT(i, j) = sprintf "DOT  %i" i
let LPAREN(i, j) = sprintf "LPAREN  %i" i
let RPAREN(i, j) = sprintf "RPAREN  %i" i
let LBRACK(i, j) = sprintf "LBRACK  %i" i
let RBRACK(i, j) = sprintf "RBRACK  %i" i
let LBRACE(i, j) = sprintf "LBRACE  %i" i
let RBRACE(i, j) = sprintf "RBRACE  %i" i
let SEMICOLON(i, j) = sprintf "SEMICOLON  %i" i
let COLON(i, j) = sprintf "COLON  %i" i
let COMMA(i, j) = sprintf "COMMA  %i" i

let AND(i, j) = sprintf "AND  %i" i
let OR(i, j) = sprintf "OR  %i" i
let TIMES(i, j) = sprintf "TIMES  %i" i
let DIVIDE(i, j) = sprintf "DIVIDE  %i" i
let PLUS(i, j) = sprintf "PLUS  %i" i
let MINUS(i, j) = sprintf "MINUS  %i" i
let EQ(i, j) = sprintf "EQ  %i" i
let NEQ(i, j) = sprintf "NEQ  %i" i
let GT(i, j) = sprintf "GT  %i" i
let GE(i, j) = sprintf "GE  %i" i
let LT(i, j) = sprintf "LT  %i" i
let LE(i, j) = sprintf "LE  %i" i

let STRING(s, i, j) = sprintf "STRING(%s)  %i " s i
let INT(c, i, j) = sprintf "INT(%i)  %i, %i" c i j
let ID(s,i,j) = sprintf "ID(%s)  %i" s i
let EOF(i, j) = sprintf "EOF  %i" i
