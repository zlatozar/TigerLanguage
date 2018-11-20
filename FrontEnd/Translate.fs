module Translate

open Tree
open Frame

// ____________________________________________________________________________
//                                                                       Types

// Result type
type Exp =
    | Ex of Tree.Exp   // value that could be assigned
    | Nx of Tree.Stm   // no value, can't assing
    | Cx of CxFunc     // represents condition

// Function is used to postpone the execution. Needed parameters are known at runtime.
and CxFunc = Temp.Label * Temp.Label -> Tree.Stm

// Main structure
type Level =
    | Top
    | Inner of  InnerRec * unit ref

and InnerRec = { parent: Level; frame: Frame.Frame }

// Here Level(because of nesting) should be considered
type Access = Level * Frame.Access

// ____________________________________________________________________________
//                                                            Helper functions

let private blockCode stmList =
    let rec cons x xs =
        match xs with
        | []          -> x
        | [s]         -> SEQ (x, s)
        | stm :: stms -> SEQ (x, cons stm stms)

    match stmList with
    | []      -> failwithf "ERROR: Instruction chunk can't be empty."
    | x :: xs -> cons x xs

let unEx e =
    match e with
    | Ex exp        -> exp
    | Cx genStmFunc -> let r = Temp.newTemp
                       let t = Temp.newLabel
                       let f = Temp.newLabel

                       ESEQ(blockCode [ MOVE(TEMP r, CONST 1); // side effect
                                         genStmFunc(t, f);

                                       LABEL f;
                                         MOVE(TEMP r, CONST 0);

                                       LABEL t],
                            TEMP r)                             // result

    | Nx s          -> ESEQ(s, CONST 0)

let unNx e =
    match e with
    | Ex exp        -> EXP exp
    | Cx genStmFunc -> let t = Temp.newLabel
                       genStmFunc(t, t) |> ignore // call function and go to the end
                       LABEL t
    | Nx s          -> s

let unCx e =
    match e with
    | Ex (CONST 0)  -> fun _ f -> JUMP (NAME f, [f])  // Tip: This two are needed because CJUMP requires two expressions
    | Ex (CONST 1)  -> fun t _ -> JUMP (NAME t, [t])  //      but we have only one
    | Ex exp        -> fun t f -> CJUMP (NE, exp, CONST 0, f, t)
    | Cx genStmFunc -> fun t f -> genStmFunc(t, f)
    | Nx _          -> failwithf "ERROR: Impossible usage of unCx."

let outermost = Top

type FuncInfo = { parent: Level; name: Temp.Label; formals: bool list }

// When enter a new inner function
let newLevel (funInfo: FuncInfo) =
    Inner ({ parent=funInfo.parent;
             frame=Frame.newFrame {name=funInfo.name; formalsEsc=true::funInfo.formals}
           }, ref () )

// Return formals associated with the frame in this level,
// excluding the static link (first element of the list p. 127)
let formals (level: Level) = match level with
                             | Top                -> []
                             | Inner(innerRec, _) -> let formalParams = List.tail innerRec.frame.formals
                                                     List.map (fun x -> (level, x)) formalParams
let allocLocal (level: Level) escape =
    match level with
    | Top                 -> failwithf "ERROR: locals can't exist on top level."
    | Inner (innerRec, _) -> (level, Frame.allocLocal innerRec.frame escape)

// ____________________________________________________________________________
//                                             Intermediate Representation (IR)

// Contains list of defined procedures or strings
let fragList :Frame.Frag list ref =
    ref []

// IR is independent of the details of the source language.

// Tip: How to design Translate.fs? Specify how every Tiger language construction should be translated.

// CONST represents integer
let intIR (n: int) :Exp = Ex (CONST n)

// nil is just CONS 0
let nilIR :Exp = Ex (CONST 0)

// Try to find same string to reuse it or create new one
let strIR (newString: string) :Exp =
    let sameStr = List.tryFind (fun x -> match x with
                                         | Frame.PROC _                    -> false
                                         | Frame.STRING(_, existingString) -> newString = existingString) !fragList
    match sameStr with
    | Some(Frame.STRING(existingLabel, _)) -> Ex (NAME existingLabel)

    | _                                    -> let newLabel = Temp.newLabel
                                              fragList := (Frame.STRING(newLabel, newString) :: !fragList)
                                              Ex (NAME newLabel)

// Binary and Relational
let binopIR (oper, e1, e2) :Exp =
    let left = unEx(e1)
    let right = unEx(e2)

    match oper with
    | Absyn.PlusOp   -> Ex (BINOP (PLUS, left, right))
    | Absyn.MinusOp  -> Ex (BINOP (MINUS, left, right))
    | Absyn.TimesOp  -> Ex (BINOP (MUL, left, right))
    | Absyn.DivideOp -> Ex (BINOP (DIV, left, right))
    | _              -> failwithf "ERROR: binary operator is expected not a relational."

let relopIR (oper, e1, e2) :Exp =
    let left = unEx(e1)
    let right = unEx(e2)

    match oper with
    // depends from the result (late binding) so use a function
    | Absyn.EqOp   -> Cx (fun (t, f) -> CJUMP (EQ, left, right, t, f))
    | Absyn.NeqOp  -> Cx (fun (t, f) -> CJUMP (NE, left, right, t, f))
    | Absyn.LtOp   -> Cx (fun (t, f) -> CJUMP (LT, left, right, t, f))
    | Absyn.LeOp   -> Cx (fun (t, f) -> CJUMP (LE, left, right, t, f))
    | Absyn.GtOp   -> Cx (fun (t, f) -> CJUMP (GT, left, right, t, f))
    | Absyn.GeOp   -> Cx (fun (t, f) -> CJUMP (GE, left, right, t, f))
    | _            -> failwithf "ERROR: relational operator is expected not a binary."

 // Fetch static links between the level of use (the level passed to simpleVarIR)
 // and the level of definition - the level within the variable's access
let simpleVarIR (access, varUsedLevel) :Exp =
    let (defLevel, defAccess) = access

    match defLevel with
    | Top              -> failwithf "ERROR: can't pass Top level as current."
    | Inner(_, defRef) -> let rec iter(curLevel, tempFP) =
                              match curLevel with
                              | Top                     -> failwithf "ERROR: failed to find level."
                              | Inner(curLevel, curRef) -> if (defRef = curRef)
                                                               then Frame.exp(defAccess) tempFP
                                                               else let staticlink = List.head (Frame.formals curLevel.frame)
                                                                    iter(curLevel.parent, Frame.exp(staticlink) tempFP)
                          // access is defined as offset from the FP p. 156
                          Ex (iter(varUsedLevel, TEMP(Frame.FP)))