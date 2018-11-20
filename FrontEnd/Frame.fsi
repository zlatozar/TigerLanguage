module Frame

// Frame is a data structure holding:
//   - the locations of all the formals
//   - instructions required to implement the "view shift"
//   - the number of locals allocated so far
//   - the `label` at which the function's machine code is to begin (see p.140)

// This full version is on p. 260

// ____________________________________________________________________________
//                                                                       Types

type Access =
    | InFrame of int
    | InReg of Temp.Temp

// What the frame should contain
type Frame = { name: Temp.Label; formals: Access list;
               locals: int ref; instrs: Tree.Stm list }

// Regeters are named
type Register = string

// Memory fragment could contain string or a routine
type Frag =
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec =
    { body: Tree.Stm; frame: Frame }


// Architecture word size
val WORDSIZE : int

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

val exp : Access -> Tree.Exp -> Tree.Exp

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

// Create new frame
val newFrame   : FrameRec -> Frame

// Allocate local variable and return it's location
val allocLocal : Frame -> bool -> Access

// Routine name
val name       : Frame -> Temp.Label

// Rotine formal parameters (where to find them)
val formals   : Frame -> Access list

val tempMap   : Register Temp.Table
val tempName  : Temp.Temp -> string

val string : Tree.Label * string -> string

val externalCall : string * Tree.Exp list -> Tree.Exp

// Executing code when enter the procedure and on exit

// val procEntryExit1 : Frame * Tree.Stm -> Tree.Stm

// val procEntryExit2 : frame * Assem.Instr list -> Assem.Instr list
// val procEntryExit3 : frame * Assem.Instr list -> { prolog: string, body: Assem.instr list, epilog: string }
