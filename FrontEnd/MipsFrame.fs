module Frame

type Access =
    | InFrame of int
    | InReg of Temp.Temp

type Frame = { name: Temp.Label; formals: Access list;
               locals: int ref; instrs: Tree.Stm list }

type Register = string

type Frag =
    | PROC of ProcRec
    | STRING of Temp.Label * string

and ProcRec =
    { body: Tree.Stm; frame: Frame}

type FrameRec = { name: Temp.Label; formalsEsc: bool list }

let wordSize = 4

// ____________________________________________________________________________
//                            All 32 MIPS instructions

(*
    $zero  = $r0        always value of 0
    $v0-v1 = $r2-r3     return values (only using 1 here)
    $a0-a3 = $r4-r7     function args
    $t0-t7 = $r8-r15    temps (caller saves)
    $s0-s7 = $r16-23    saved temps (callee saves)
    $t8-t9 = $r24-25    temps (caller saves)

    $sp = $r29          stack pointer
    $fp = $r30          frame pointer
    $ra = $r31          return address
*)

let R0 = Temp.newTemp // always ZERO
let AT = Temp.newTemp // assembler temporary, reserved

let RV = Temp.newTemp // return value from procedure call
let V1 = Temp.newTemp

// Used to pass the first four arguments to routines

let A0 = Temp.newTemp
let A1 = Temp.newTemp
let A2 = Temp.newTemp
let A3 = Temp.newTemp

// Temporary - not preserved across call

let T0 = Temp.newTemp
let T1 = Temp.newTemp
let T2 = Temp.newTemp
let T3 = Temp.newTemp
let T4 = Temp.newTemp
let T5 = Temp.newTemp
let T6 = Temp.newTemp
let T7 = Temp.newTemp

// Saved temporary - preserved across call

let S0 = Temp.newTemp
let S1 = Temp.newTemp
let S2 = Temp.newTemp
let S3 = Temp.newTemp
let S4 = Temp.newTemp
let S5 = Temp.newTemp
let S6 = Temp.newTemp
let S7 = Temp.newTemp

let T8 = Temp.newTemp
let T9 = Temp.newTemp

// Reserved for kernel
let K0 = Temp.newTemp
let K1 = Temp.newTemp

let GP = Temp.newTemp
let SP = Temp.newTemp
let FP = Temp.newTemp // Frame Pointer
let RA = Temp.newTemp // Return Adderss

// ____________________________________________________________________________

// Constant 0
let ZERO = R0

let specialRegs = [
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

let argRegs = [
    (A0, "$a0");
    (A1, "$a1");
    (A2, "$a2");
    (A3, "$a3")]

let calleeSaves = [
    (S0, "$s0");
    (S1, "$s1");
    (S2, "$s2");
    (S3, "$s3");
    (S4, "$s4");
    (S5, "$s5");
    (S6, "$s6");
    (S7, "$s7")]

let callerSaves = [
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

let registers = List.map (fun (_, name) -> name) (argRegs @ callerSaves @ calleeSaves)

let newFrame (frameRec: FrameRec) = { name=(Store.symbol "dummy"); formals=[]; locals=ref 0; instrs=[] }

let name (frame: Frame) = frame.name

let formals (frame: Frame) = frame.formals

let allocLocal (frame: Frame) (escape: bool) =
    if (escape) then
        let offSet = !frame.locals + 1
        let ret = InFrame (offSet * (-wordSize)) // stack grows to lower addresses
        frame.locals := !frame.locals + 1; ret
    else InReg(Temp.newTemp)
