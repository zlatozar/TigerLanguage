module Translate

open Tree

// ____________________________________________________________________________
//                                                                       Types

type Exp =
    | Ex of Tree.Exp
    | Nx of Tree.Stm
    | Cx of CxFunc

and CxFunc = Temp.Label * Temp.Label -> Tree.Stm

type Level =
    | Top
    | Nested of  NestedRec * unit ref

and NestedRec = { parent: Level; frame: Frame.Frame }

type Access = Level * Frame.Access

// ____________________________________________________________________________
//                                                                     Helpers

val outermost : Level

type FuncInfo = { parent: Level; name: Temp.Label; formals: bool list }

val newLevel : FuncInfo -> Level

val formals : Level -> Access list

val allocLocal : Level -> bool -> Access

val fragList : Frame.Frag list ref

val newBreakpoint : string * int

// ____________________________________________________________________________
//                                                            transVar section

val simpleVarIR : Access * Level -> Exp

val fieldVarIR : Exp * int -> Exp

val subscriptVarIR : Exp * Exp -> Exp

// ____________________________________________________________________________
//                                                            transExp section

val unitExp : Exp

val intIR : int -> Exp

val strIR : string -> Exp

val nilIR : Exp

val breakIR : Temp.Label -> Exp

val assignIR : Exp * Exp -> Exp

val binopIR : Absyn.TOper * Exp * Exp -> Exp

val relopIR : Absyn.TOper * Exp * Exp -> Exp

val strEQ : Exp * Exp -> Exp

val strNEQ : Exp * Exp -> Exp

val callIR : Level * Level * Temp.Label * Exp list * bool -> Exp

val recordIR : Exp list -> Exp

val arrayIR : Exp * Exp -> Exp

val sequenceIR : Exp list -> Exp

val ifThenIR : Exp * Exp -> Exp

val ifThenElseIR : Exp * Exp * Exp -> Exp

val whileIR : Exp -> (Exp -> Exp)

val forIR : Exp * Exp * Exp -> (Exp -> Exp)

val letIR : Exp list * Exp -> Exp

val procEntryExit : Level * Exp -> unit

