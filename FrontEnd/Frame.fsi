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

type Register = string

type Frag = 
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec = 
    { body: Tree.Stm; frame: Frame}    

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

// ____________________________________________________________________________
//                                                                   Functions

val newFrame   : FrameRec -> Frame
val name       : Frame -> Temp.Label
val formals    : Frame -> Access list
val allocLocal : Frame -> bool -> Access

// val string : Tree.Label * string -> string

// val FP : Temp.Temp
// val SP : Temp.Temp
// val RV : Temp.Temp
// val RA : Temp.Temp

//val wordSize : int

// val exp : Access -> Tree.Exp -> Tree.Exp

// val registers : Register list

// val argRegs     : Temp.Temp list
// val callerSaves : Temp.Temp list
// val calleeSaves : Temp.Temp list

// val procEntryExit1 : Frame * Tree.Stm -> Tree.Stm
// val procEntryExit2 : frame * Assem.Instr list -> Assem.Instr list
// val procEntryExit3 : frame * Assem.Instr list -> { prolog: string, body: Assem.instr list, epilog: string }

// val tempMap   : Register Temp.Table
// val tempName  : Temp.Temp -> string

// val externalCall : string * Tree.Exp list -> Tree.Exp
