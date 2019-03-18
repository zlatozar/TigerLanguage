module Frame

open Tree

type Access =
    | InFrame of int
    | InReg of Temp.Temp

type Frame = { name: Temp.Label; formals: Access list; curOffset: int ref }

type Register = string

// Fragmet is a procedure in a frame or a string.
// Existing fragments could be reused.
type Frag =
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec =
    { body: Tree.Stm; frame: Frame }

// Tip: The static link escapes so it needs to be kept in memory

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

let WORDSIZE = 4

let private blockCode stmList =
    let rec cons x xs =
        match xs with
        | []          -> x
        | [s]         -> SEQ (x, s)
        | stm :: stms -> SEQ (x, cons stm stms)

    match stmList with
    | []      -> EXP (CONST 0)
    | x :: xs -> cons x xs

// ____________________________________________________________________________
//                            All 32 MIPS instructions

(*  Here is what could be used:

    $zero  = $r0        always value of 0
    $v0-v1 = $r2-r3     return values (only use only V0)
    $a0-a3 = $r4-r7     function args
    $t0-t7 = $r8-r15    temps (caller saves)
    $s0-s7 = $r16-23    saved temps (callee saves)
    $t8-t9 = $r24-25    temps (caller saves cont.)

    $sp = $r29          stack pointer
    $fp = $r30          frame pointer
    $ra = $r31          return address
*)

let R0 = Temp.newTemp() // always zero
let AT = Temp.newTemp() // assembler temporary, reserved

let RV = Temp.newTemp() // return value from procedure call
let V1 = Temp.newTemp()

// Used to pass the first four arguments to routines

let A0 = Temp.newTemp()
let A1 = Temp.newTemp()
let A2 = Temp.newTemp()
let A3 = Temp.newTemp()

// Temporary - not preserved across call

let T0 = Temp.newTemp()
let T1 = Temp.newTemp()
let T2 = Temp.newTemp()
let T3 = Temp.newTemp()
let T4 = Temp.newTemp()
let T5 = Temp.newTemp()
let T6 = Temp.newTemp()
let T7 = Temp.newTemp()

// Saved temporary - preserved across call

let S0 = Temp.newTemp()
let S1 = Temp.newTemp()
let S2 = Temp.newTemp()
let S3 = Temp.newTemp()
let S4 = Temp.newTemp()
let S5 = Temp.newTemp()
let S6 = Temp.newTemp()
let S7 = Temp.newTemp()

let T8 = Temp.newTemp()
let T9 = Temp.newTemp()

// Reserved for kernel
let K0 = Temp.newTemp()
let K1 = Temp.newTemp()

let GP = Temp.newTemp() // Pointer to global area
let SP = Temp.newTemp() // Stack Pointer
let FP = Temp.newTemp() // Frame Pointer
let RA = Temp.newTemp() // Return Adderss

// ____________________________________________________________________________

// Constant 0
let ZERO = R0

let specialRegsMap = [
    (RV, "$v0");
    (V1, "$v1");
    (R0, "$zero");
    (AT, "$at");
    (K0, "$k0")
    (K1, "$k1");
    (GP, "$gp");
    (SP, "$sp");
    (FP, "$fp");
    (RA, "$ra")]

let argRegsMap = [
    (A0, "$a0");
    (A1, "$a1");
    (A2, "$a2");
    (A3, "$a3")]

let argRegsNum = List.length argRegsMap
let argRegs = List.map (fun (r, _) -> r) argRegsMap

// May be NOT overritten by called procedures
let calleeSavesMap = [
    (S0, "$s0");
    (S1, "$s1");
    (S2, "$s2");
    (S3, "$s3");
    (S4, "$s4");
    (S5, "$s5");
    (S6, "$s6");
    (S7, "$s7")]

let calleeSaveRegs = List.map (fun (temp, _) -> temp) calleeSavesMap

// May be overritten by called procedures
let callerSavesMap = [
    (T0, "$t0");
    (T1, "$t1");
    (T2, "$t2");
    (T3, "$t3");
    (T4, "$t4");
    (T5, "$t5");
    (T6, "$t6");
    (T7, "$t7");
    (T8, "$t8");
    (T9, "$t9")]

let callerSaveRegs = List.map (fun (temp, _) -> temp) callerSavesMap

// A list of all register name, which can be used for coloring
let registers = List.map (fun (_, name) -> name) (argRegsMap @ callerSavesMap @ calleeSavesMap)

let tempMap =
    (specialRegsMap @ argRegsMap @ callerSavesMap @ calleeSavesMap) |>
        List.fold (fun table (k, v) -> Temp.Table.enter table k v) Temp.Table.empty

let tempName t =
    match Temp.Table.look tempMap t with
    | Some name -> name
    | None      -> Temp.makeString t

(*
  MIPS frame

    ...                        higher addresses
|   arg_N    |  4 + 4*N
|   ...      |
|   arg_1    |  8               CALLER frame
|static link |  4
|            |
|saved old FP|  FP              -------------
|            |
|  local_1   |  FP - 4
|  local_2   |  FP - 8
|   ...      |
|  local_N   |  FP - 4*N
|            |
|return addr |                 CALLEE (current) frame
|            |
|temporaries |
|            |
|saved regs  |
                               lower addresses
*)

// Turns a Frame.Access into the Tree expression.
// Use it to reach a variable/expression result.
//
// For a simple variable 'v' declared in the current procedure's stack frame,
// 'k' is the offset of 'v' within the frame and 'fp' is the frame pointer register (p. 154).
let exp loc fp :Exp =
    match loc with
    | InFrame(k) -> MEM (BINOP (PLUS, fp, CONST k))
    | InReg(r)   -> TEMP r

// First slot is to save old $fp
let initialOffset = -4

// 'newFrame' must calculate two things:
//   1. How the parameter will be seen from inside the function (in a register, or in a frame location);
//   2. What instructions must be produced to implement the "view shift."
let newFrame (frameRec: FrameRec) =
    let offset = ref initialOffset

    let rec allocLocals formalsEsc =
        match formalsEsc with
        | []   -> []
        | h::t -> // Local allocs on top of stack starting from 0
                  let access = if h
                                   then
                                        let ret = InFrame(!offset)
                                        offset := !offset - WORDSIZE
                                        ret
                                   else InReg(Temp.newTemp())

                  access :: allocLocals t
    let formalAccesses = allocLocals frameRec.formalsEsc
    { name = frameRec.name; formals = formalAccesses; curOffset = offset }

// Allocate a local variable in given frame or in a register if not escapes
let allocLocal (frame: Frame) (escape: bool) =

    if (escape) then
        // grows from high addr to low addr
        let offSet = !frame.curOffset - WORDSIZE
        InFrame(offSet)

    else InReg(Temp.newTemp())

let name (frame: Frame) = frame.name

let formals (frame: Frame) = frame.formals

let string (label, str) = sprintf ".data\n %s: .asciiz \"%s\"\n.text\n\n" (Store.name label) str

let externalCall (name, args) = CALL (NAME (Temp.namedLabel name), args)

// For each incoming register parameter, move it to the place
// from which it is seen from within the function.
//
// NOTICE: bodyStm already include move to save bodyResult to RV register
let procEntryExit1 (frame: Frame, bodyStm: Stm) :Stm =
    let saveToFrame = true

    let argsMoves =
        let allocArgToLoc i access =
            let dstLoc = exp access (TEMP FP)
            let argOffset = i * WORDSIZE          // Starting at -16 (initialOffset * 4)
            if i < 3
                then
                    MOVE (dstLoc, TEMP (List.item i argRegs))
                else
                    MOVE (dstLoc, MEM (BINOP(PLUS, TEMP(FP), CONST(argOffset))))

        List.mapi allocArgToLoc frame.formals

    let returnAddress = exp (allocLocal frame saveToFrame) (TEMP FP)
    let saveRA = MOVE (returnAddress, TEMP RA)
    let restoreRA = MOVE (TEMP RA, returnAddress)

    let regLocMapping = List.map (fun reg -> (reg, exp (allocLocal frame saveToFrame) (TEMP FP))) calleeSaveRegs

    let generateSaveMove = function (reg, loc) ->  MOVE (loc, TEMP reg)
    let generateRestoreMove = function (reg, loc) -> MOVE (TEMP reg, loc)

    let calleeSaveMoves = List.map generateSaveMove regLocMapping
    let calleeRestoreMoves = List.map generateRestoreMove regLocMapping

    blockCode (calleeSaveMoves @ [saveRA] @ argsMoves @ [bodyStm] @ calleeRestoreMoves @ [restoreRA])