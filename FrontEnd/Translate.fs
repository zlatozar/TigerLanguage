module Translate

open Tree

// ____________________________________________________________________________
//                                                                       Types

// Result type
type Exp =
    | Ex of Tree.Exp
    | Nx of Tree.Stm
    | Cx of CxFunc

and CxFunc = Temp.Label * Temp.Label -> Tree.Stm

// Main structure
type Level =
    | Top
    | Inner of  InnerRec * unit ref

and InnerRec = { parent: Level; frame: Frame.Frame }

// Here Level(because of nesting) should be considered
type Access = Level * Frame.Access

// ____________________________________________________________________________
//    Functions used by sematinc analysis to translate to intermidate language (dummy)

// Tip: How to design Translate.fs? Specify how every Tiger language construction should be translated.

let instrChunk stmList =
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

                       ESEQ(instrChunk [ MOVE(TEMP r, CONST 1); // side effect
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
                       genStmFunc(t, t) |> ignore // call given function
                       LABEL t
    | Nx s          -> s

let outermost = Top

type FuncInfo = { parent: Level; name: Temp.Label; formals: bool list }

let newLevel (funInfo: FuncInfo) = Top

let formals (level: Level) = match level with
                             | Top                -> []
                             | Inner(innerRec, _) -> let formParams = innerRec.frame.formals
                                                     List.map (fun x -> (level, x)) formParams

let allocLocal (level: Level) escape =
    match level with
    | Top                 -> failwithf "ERROR: locals can't exist on top level."
    | Inner (innerRec, _) -> (level, Frame.allocLocal innerRec.frame escape)

