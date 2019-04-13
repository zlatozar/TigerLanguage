module Frame

// Frame is a data structure holding:
//   - the locations of all the formals
//   - instructions required to implement the "view shift" for arguments
//   - the number of locals allocated so far
//   - the `label` at which the function's machine code is to begin (see p.140)

// Contains machine-dependant definitions

// ____________________________________________________________________________
//                                                                       Types

type Access =
    | InFrame of int
    | InReg of Temp.Temp

// What the routine frame should contain
type Frame = { name: Temp.Label; formals: Access list; fpaccess: Access;
               mutable allocated: int; viewshift: Tree.Stm list; mutable maxOutgoing: int }


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

val argRegs         : Temp.Temp list
val calleeSavesRegs : Temp.Temp list
val callerSavesRegs : Temp.Temp list

// List of all register name, which can be used for coloring
val registers : Register list

// Tip: A variable escapes if its declared in a higher function and is used in a lower function.

val exp : Access -> Tree.Exp -> Tree.Exp

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

// Create new frame
val newFrame   : FrameRec -> Frame

// Give register or allocate memory with one machine word size and return it's location
val allocFrameLocal : Frame -> bool -> Access

// Routine name is used as label
val name       : Frame -> Temp.Label

// Routine formal parameters (where to find them)
val formals   : Frame -> Access list

val tempMap   : Register Temp.Table
val tempName  : Temp.Temp -> string

val string : Tree.Label * string -> string

val externalCall : string * Tree.Exp list -> Tree.Exp

// Defines what should be done when executing a routine

val procEntryExit1 : Frame * Tree.Stm -> Tree.Stm

// val procEntryExit2 : frame * Assem.Instr list -> Assem.Instr list
// val procEntryExit3 : frame * Assem.Instr list -> { prolog: string, body: Assem.instr list, epilog: string }
