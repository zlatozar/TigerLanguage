module TigerSemantic

open Absyn

open Types
open Env
open ErrorMsg

// Intermediate language expression and its type (next chapters)
type ExpTy = {exp: Translate.Exp; ty: Types.Ty}

// Variables
type VEnv = Store.Table<VarEntry>

// Functions
type FEnv = Store.Table<FunEntry>

// Types
type TEnv = Store.Table<Types.Ty>

type ProgEnv = {venv: VEnv; fenv: FEnv; tenv: TEnv}

let dummyTransExp = { exp=(); ty=Types.INT }

// let transVar: venv * tenv * Absyn.var -> expty
// let transExp: venv * tenv * Absyn.exp -> expty
// let transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
// let transTy: tenv * Absyn.ty -> Types.ty

// ______________________________________________________________________________
//                                                              Helper functions

// (NAME a, (NAME b, (NAME c, T)))
let rec actualTy (tenv, ty: Ty) =
    match ty with
    | NAME (sym, opTy) -> match !opTy with
                          | None    -> match Store.lookup (tenv, sym) with
                                       | None     -> ty  // was a placeholder - fill it
                                       | Some ty' -> opTy := Some ty'
                                                     actualTy (tenv, ty')
                          | Some ty ->  actualTy (tenv, ty)

    | ARRAY (ty', u)   -> ARRAY (actualTy (tenv, ty'), u)
    | _                -> ty

// ______________________________________________________________________________
//

// Tip: Use Prettyprint.fs as scaffold

let rec transVar (venv, tenv, (var: Absyn.TVar)) =
    match var with
    | SimpleVar (sym, pos)        -> match Store.lookup (venv, sym) with
                                     | None   -> error pos (sprintf "undefined variable '%s'" (Store.name sym))
                                                 dummyTransExp
                                     | Some v -> { exp=(); ty=actualTy (tenv, v.ty) }

    | FieldVar (tVar, sym, pos)   -> dummyTransExp
    | SubscriptVar (tVar, e, pos) -> dummyTransExp

and transExp (venv, tenv, (exp: Absyn.TExp)) =
    match exp with
    | IntExp i             -> dummyTransExp
    | StringExp (str, pos) -> dummyTransExp
    | NilExp               -> dummyTransExp
    | BreakExp pos         -> dummyTransExp
    | VarExp tVar          -> dummyTransExp

    | AssignExp assignRec  -> dummyTransExp
    | OpExp opRec       -> dummyTransExp
    | CallExp callRec   -> dummyTransExp
    | RecordExp recRec  -> dummyTransExp
    | ArrayExp arrayRec -> dummyTransExp
    | SeqExp expList    -> dummyTransExp
    | IfExp ifRec       -> dummyTransExp
    | WhileExp whileRec -> dummyTransExp
    | ForExp forRec     -> dummyTransExp
    | LetExp letRec     -> dummyTransExp

and transDec (venv, tenv, (dec: Absyn.TDec)) =
    match dec with
    | TypeDec typeRecList    -> dummyTransExp
    | VarDec varRec          -> dummyTransExp
    | FunctionDec funDecList -> dummyTransExp

and transTy (tenv, (ty: Absyn.TType)) =
    match ty with
    | NameTy (sym, pos)      -> dummyTransExp
    | RecordTy fieldRecList  -> dummyTransExp
    | ArrayTy (sym, pos)     -> dummyTransExp
