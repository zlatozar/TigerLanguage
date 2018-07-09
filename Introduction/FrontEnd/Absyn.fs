(* Abstract syntax for Straight-line language *)

// NOTE: Punctuation should be eliminated

module Absyn

type Stmt =
    | CompoundStmt of Stmt * Stmt
    | AssignStmt of Id * Exp
    | PrintStmt of Exp list

and Exp =
    | IdExp of Id
    | NumExp of int
    | OpExp of Exp * BinOp * Exp
    | SeqExp of Stmt * Exp

and Id = string

and BinOp =
    | Plus
    | Minus
    | Times
    | Div
