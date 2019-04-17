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

// Where is defined (Access) and where is used - current level
val simpleVarIR : Access * Level -> Exp

// Record expression and position(index) of the field definition
val fieldVarIR : Exp * int -> Exp

// Array expression and index
val subscriptVarIR : Exp * Exp -> Exp

// ____________________________________________________________________________
//                                                            transExp section

// Represents `()` expression
val unitExp : Exp

// Integer constant
val intIR : int -> Exp

// String literal
val strIR : string -> Exp

// Represents `nil`
val nilIR : Exp

// Represents `break`
val breakIR : Temp.Label -> Exp

// LHS and RHS
val assignIR : Exp * Exp -> Exp

// Binary operator (+, -, *, /) and two operands
val binopIR : Absyn.TOper * Exp * Exp -> Exp

// Relational operator (=, !=, >, >=, <, <=) and two operands (strings are excluded from =, !=)
val relopIR : Absyn.TOper * Exp * Exp -> Exp

val strEQ : Exp * Exp -> Exp

val strNEQ : Exp * Exp -> Exp

// Level of usage, level of definition, function name, actual parameters, is a procedure or function
val callIR : Level * Level * Temp.Label * Exp list * bool -> Exp

// Record init - accepts the list of fields expressions
val recordIR : Exp list -> Exp

// Array init - array size and initialization value
val arrayIR : Exp * Exp -> Exp

// All expressions in a sequence
val sequenceIR : Exp list -> Exp

// Test and the consiquence expressions
val ifThenIR : Exp * Exp -> Exp

val ifThenElseIR : Exp * Exp * Exp -> Exp

val whileIR : Exp * Label -> (Exp -> Exp)

val forIR : Exp * Exp * Exp * Label -> (Exp -> Exp)

val letIR : Exp list * Exp -> Exp

// Function definition - current level and the body expression
val procEntryExit : Level * Exp -> unit

