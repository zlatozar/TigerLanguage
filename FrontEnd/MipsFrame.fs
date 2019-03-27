module Frame

open Tree

type Access =
    | InFrame of int
    | InReg of Temp.Temp

type Frame = { name: Temp.Label; formals: Access list;
               locals: int ref; viewShiftInstr: Tree.Stm list }

type Register = string

// Fragmet is a procedure in a frame or a string.
// Existing fragments could be reused.
type Frag =
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec =
    { body: Tree.Stm; frame: Frame }

// Tip: The static link always escapes so it needs to be kept in memory

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

// 32 bit architecture
let WORDSIZE = 4

// ____________________________________________________________________________
//                            All 32 MIPS instructions

(*
    $zero  = $r0        always value of 0
    $v0-v1 = $r2-r3     return values (use only v0 as RV)
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

let RV = Temp.newTemp() // a.k.a V0, return value from function call
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

let calleeSavesRegs = List.map (fun (temp, _) -> temp) calleeSavesMap

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

let callerSavesRegs = List.map (fun (temp, _) -> temp) callerSavesMap

// A list of all register name, which can be used for coloring
let registers = List.map (fun (_, name) -> name) (argRegsMap @ callerSavesMap @ calleeSavesMap)

(*
  MIPS frame

    ...                        higher addresses
|   arg_N    |  4 + 4*N
|   ...      |
|   arg_1    |  8               CALLER frame
|static link |  4
|            |
|saved old FP|  0               -------------
|            |
|  local_1   |  FP - 4
|  local_2   |  FP - 8
|   ...      |                  CALLEE (current) frame
|  local_N   |  FP - 4*N
|            |
|return addr |
|            |
|temporaries |
|            |
| saved regs |
                               lower addresses

NOTE: For return address, RA register will be used
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

// 'newFrame' must calculate two things:
//   1. How the parameter will be seen from inside the function (in a register, or in a frame location);
//   2. What instructions must be produced arguments to be seen ("view shift")
let newFrame (frameRec: FrameRec) =
    let n = List.length frameRec.formalsEsc

    let rec placeIn (escapes, size) =
        match (escapes, size) with
        | ([], _)               -> []
        | (first::rest, offset) -> if first
                                       then InFrame(offset) :: placeIn (rest, offset + WORDSIZE)
                                       else InReg(Temp.newTemp()) :: placeIn (rest, offset)

    let funcParams :Access list = placeIn (frameRec.formalsEsc, WORDSIZE)

    // calculate offset from FP
    let calcOffset (param, reg) = MOVE (exp param (TEMP FP), TEMP reg)
    let shiftInstrs = Seq.zip funcParams argRegs   // could be with different length
                      |> Seq.map calcOffset
                      |> Seq.toList

    // For functions with more than 4 paramters, we just give up
    if n <= argRegsNum
        then { name=frameRec.name; formals=funcParams; locals=ref 0; viewShiftInstr=shiftInstrs }
        else failwithf "ERROR: Too many function arguments: %d." n

// Return one word in memory in given frame or a register if not escapes
let allocLocal (frame: Frame) (escape: bool) =
    if (escape) then
        let offSet = !frame.locals + 1
        let ret = InFrame(offSet * (-WORDSIZE))
        frame.locals := !frame.locals + 1; ret

    else InReg(Temp.newTemp())

let name (frame: Frame) = frame.name

let formals (frame: Frame) = frame.formals

let tempMap =
    (argRegsMap @ callerSavesMap @ calleeSavesMap) |>
        List.fold (fun table (k, v) -> Temp.Table.enter table k v) Temp.Table.empty

let tempName t =
    match Temp.Table.look tempMap t with
    | Some name -> name
    | None      -> Temp.makeString t

let string (label, str) =  sprintf "%s: .asciiz \"%s\"\n" (Store.name label) str

let externalCall (name, args) = CALL (NAME (Temp.namedLabel name), args)

let private blockCode stmList =
    let rec cons x xs =
        match xs with
        | []          -> x
        | [s]         -> SEQ (x, s)
        | stm :: stms -> SEQ (x, cons stm stms)

    match stmList with
    | []      -> EXP (CONST 0)
    | x :: xs -> cons x xs

// For each incoming register parameter, move it to the place
// from which it is seen from within the function. This could be
// a frame location (for escaping parameters) or a fresh temporary.
//
// ATTENTION: In this **first** version is not optimal and there is a lot of room for optimization.
let procEntryExit1 (frame: Frame, body: Stm) :Stm =
    let args = frame.viewShiftInstr   // see args using "veiw shift" instructions

    let pairs = List.map (fun reg -> (allocLocal frame false, reg)) (RA::calleeSavesRegs)
    let savedRegs = List.map (fun (localInFrame, reg) -> MOVE (exp localInFrame (TEMP FP), TEMP reg)) pairs
    let restores =
        List.map (fun (localInFrame, reg) -> MOVE (TEMP reg, exp localInFrame (TEMP FP))) (List.rev pairs)

    blockCode (args @ savedRegs @ [body] @ restores)