module Translate

// ____________________________________________________________________________
//                                                                       Types

// Dummy
type Exp = 
    | Ex of Tree.Exp
    | Nx of Tree.Stm
    | Cx of CxFunc 

and CxFunc = Temp.Label * Temp.Label -> Tree.Stm    

type Level =  
    | Top
    | Lev of  LevRec * unit ref

and LevRec = { parent: Level; frame: Frame.Frame }

type Access = Level * Frame.Access

type FuncInfo = { parent: Level; name: Temp.Label ; formals: bool list }

// ____________________________________________________________________________
//                                                           Functions (dummy)

// Tip: How to design Translate.fs? Specify how every Tiger language construction should be translated.

let outermost = Top

let newLevel (funInfo: FuncInfo) = Top

let formals (level: Level) = match level with
                             | Top             -> []
                             | Lev (levRec, _) -> let formParams = levRec.frame.formals
                                                  List.map (fun x -> (level, x)) formParams

let allocLocal (level: Level) escape = 
    match level with
    | Top             -> failwithf "ERROR: `allocLocal` passed Top level."     
    | Lev (levRec, _) -> (level, Frame.allocLocal levRec.frame escape)                                       

