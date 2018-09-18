module Frame

// This full version is on p. 260

// ____________________________________________________________________________
//                                                                       Types

type Access =
    | InFrame of int
    | InReg of Temp.Temp

// Outline frame structure
type Frame = { name: Temp.Label; formals: Access list;
               locals: int ref; instrs: Tree.Stm list }

// Regeters are named
type Register = string

type Frag =
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec =
    { body: Tree.Stm; frame: Frame}


// Architecture word size
val wordSize : int

// ____________________________________________________________________________
//

val FP : Temp.Temp
val SP : Temp.Temp
val RV : Temp.Temp
val RA : Temp.Temp

val argRegs     : (Temp.Temp * Register) list
val callerSaves : (Temp.Temp * Register) list
val calleeSaves : (Temp.Temp * Register) list

// List of all register name, which can be used for coloring
val registers : Register list

// Tip:  A variable escapes if its declared in a higher function and is used in a lower function.

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

val newFrame   : FrameRec -> Frame
val name       : Frame -> Temp.Label
val formals    : Frame -> Access list
val allocLocal : Frame -> bool -> Access

// val tempMap   : Register Temp.Table
// val tempName  : Temp.Temp -> string

// val string : Tree.Label * string -> string

// val exp : Access -> Tree.Exp -> Tree.Exp

// val procEntryExit1 : Frame * Tree.Stm -> Tree.Stm
// val procEntryExit2 : frame * Assem.Instr list -> Assem.Instr list
// val procEntryExit3 : frame * Assem.Instr list -> { prolog: string, body: Assem.instr list, epilog: string }

// val externalCall : string * Tree.Exp list -> Tree.Exp
