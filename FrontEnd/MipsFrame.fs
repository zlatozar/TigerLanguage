module Frame

open Tree

type Access =
    | InFrame of int
    | InReg of Temp.Temp

type Frame = { name: Temp.Label; formals: Access list; fpaccess: Access;
               mutable allocated: int; viewshift: Tree.Stm list; mutable maxOutgoing: int }

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
    $s0-s7 = $r16-23    preserved across calls temps (callee-saves)
    $t8-t9 = $r24-25    temps (caller-saves cont.)

    $sp = $r29          stack pointer
    $fp = $r30          frame pointer
    $ra = $r31          return address
*)

let R0 = Temp.newTemp() // always zero
let AT = Temp.newTemp() // assembler temporary, reserved

let V0 = Temp.newTemp() // return value from function call
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

let RV = V0
let GP = Temp.newTemp() // Pointer to global area
let SP = Temp.newTemp() // Stack Pointer
let FP = Temp.newTemp() // Frame Pointer
let RA = Temp.newTemp() // Return Adderss

// ____________________________________________________________________________

// Constant 0
let ZERO = R0

let specialRegsMap = [
    (RV, "$v0");
    (R0, "$zero");
    (AT, "$at");
    (K0, "$k0")
    (K1, "$k1");
    (GP, "$gp");
    (SP, "$sp");
    (FP, "$fp");
    (RA, "$ra")]

let specialRegs = List.map (fun (r, _) -> r) specialRegsMap

let argRegsMap = [
    (A0, "$a0");
    (A1, "$a1");
    (A2, "$a2");
    (A3, "$a3")]

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
    (T9, "$t9");
    (V1, "$v1")]

let callerSavesRegs = List.map (fun (temp, _) -> temp) callerSavesMap

// A list of all register name, which can be used for coloring
let registers = List.map (fun (_, name) -> name) (argRegsMap @ callerSavesMap @ calleeSavesMap)

(*
  MIPS frame

    ...                        higher addresses

|   arg_N    |  4 + 4*N   <---- incomming args that could be seen using "view shift" code
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
|            |
|  arg2      |           <---- outgoing args
|  arg1      |
| static link|
                               lower addresses

NOTE: For return address, RA register will be used
*)

// Turns a Frame.Access into the Tree expression.
// Use it to reach a variable/expression result.
//
// NOTE: 'fp' is FP only when accessing variable from its own level
//       in other cases it's an offset from FP.
let exp access fp :Exp =
    match access with
    | InFrame(k) -> MEM (BINOP (PLUS, fp, CONST k))
    | InReg(r)   -> TEMP r

// 'newFrame' must calculate two things:
//   1. How the parameter will be seen from inside the function (in a register, or in a frame location);
//   2. What instructions must be produced arguments to be seen ("view shift")
let newFrame (frameRec: FrameRec) =
    let calcAccess access = exp access (Tree.TEMP FP)

    let allocateFormals formalParams =

        let rec allocFormal i allocated viewshift accesses = function
            | []                -> (allocated, List.rev viewshift, List.rev accesses)
            | formal :: formals -> let place = if i < List.length argRegs
                                                    then InReg (List.item i argRegs)
                                                    else InFrame ((i - List.length argRegs + 1) * WORDSIZE)

                                   let argAcc = if formal then InFrame (-allocated * WORDSIZE) else InReg (Temp.newTemp())
                                   let instr = Tree.MOVE (calcAccess argAcc, calcAccess place)

                                   allocFormal (i + 1) (if formal then allocated + 1 else allocated) (instr :: viewshift) (argAcc :: accesses) formals

        allocFormal 0 0 [] [] formalParams

    let (allocated, viewshift, formals) = allocateFormals frameRec.formalsEsc
    { name=frameRec.name; allocated=allocated + 1; viewshift=viewshift; formals=formals;
      maxOutgoing=0; fpaccess=InFrame (-allocated * WORDSIZE) }

// Return one word in memory in given frame or a register if not escapes
let allocFrameLocal (frame: Frame) escape =
    if escape then
        let access = InFrame(-frame.allocated  * WORDSIZE)
        frame.allocated <- frame.allocated + 1
        access

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


// Representation of strings that serves well is to have a string pointer point
// to a one-word integer containing the length (number of characters), followed
// immediately by the characters themselves p. 163
let string (label, s) =
    let size = String.length s

    let bytes :int array =
        Array.zeroCreate (size + WORDSIZE)

    // Why?
    Array.set bytes 0 (size &&& 0x000000ff)
    Array.set bytes 1 ((size &&& 0x0000ff00) >>> 8)
    Array.set bytes 2 ((size &&& 0x00ff0000) >>> 16)
    Array.set bytes 3 ((size &&& 0xff000000) >>> 24)

    String.iteri (fun i c -> Array.set bytes (i + WORDSIZE) ((int) c)) s

    let byteString =
        Array.fold (fun bs b -> sprintf "%s%s " bs ((string) b)) "" bytes

    sprintf ".data\n.align 2\n%s:\n\t.byte %s \n.text\n" (Store.name label) byteString

let externalCall (name, args) = CALL (NAME (Temp.namedLabel name), args)

let private blockCode stmList =
    let rec cons x xs =
        match xs with
        | []          -> x
        | [s]         -> SEQ (x, s)
        | stm :: stms -> SEQ (x, cons stm stms)

    match stmList with
    | []      -> failwith "ERROR: Block code should be not empty."
    | x :: xs -> cons x xs

// Defines what should be done before execute function body and after it's exit.
//
// ATTENTION: This first version is not optimal and there is a lot of room for optimization.
let procEntryExit1 (frame: Frame, body: Stm) :Stm =
    let args = frame.viewshift   // how to "see" function actual parameters

    let localsRA = List.map (fun reg -> (allocFrameLocal frame false, reg)) (calleeSavesRegs @ [RA])
    let calleeSaveRegs = List.map (fun (localVar, reg) -> MOVE (exp localVar (TEMP FP), TEMP reg)) localsRA
    let restoreCalleeSaveRegs =
        List.map (fun (localInFrame, reg) -> MOVE (TEMP reg, exp localInFrame (TEMP FP))) (List.rev localsRA)

    blockCode (args @ calleeSaveRegs @ [body] @ restoreCalleeSaveRegs)