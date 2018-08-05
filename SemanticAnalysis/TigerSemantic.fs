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

// Use it to continue analysis
let dummyTransExp = { exp=(); ty=Types.INT }

// ______________________________________________________________________________
//                                                              Helper functions

let checkInt ((ty: Ty), pos) =
    if ty = INT then ()
    else error pos "expected an integer"

let checkString ((ty: Ty), pos) =
    if ty = STRING then ()
    else error pos "expected a string"

let checkBothInt (ty1, ty2, pos) =
    checkInt (ty1, pos)
    checkInt (ty2, pos)

let checkBothIntOrString (ty1, ty2, pos) =
    match (ty1, ty2) with
    | (INT, INT)       -> ()
    | (INT, _)         -> error pos "expected an integer."
    | (STRING, STRING) -> ()
    | (STRING, _)      -> error pos "expected a string."
    | _                -> error pos "expecting an integer or string."

// Tip: NIL is a type and it is equal to every other
let checkBothEq (ty1, ty2, pos) =
    match (ty2, ty2) with
    | (INT,    _)       -> checkBothIntOrString (ty1, ty2, pos)
    | (STRING, _)       -> checkBothIntOrString (ty1, ty2, pos)

    | (RECORD (fields1, _), RECORD (fields2, _))
                        -> if fields1 = fields2 then ()
                           else error pos "records are not of the same type."

    | (RECORD _ , NIL) -> ()
    | (NIL, RECORD _)  -> ()
    | (RECORD _, _)    -> error pos "expecting a record."

    | (ARRAY (aty1, _), ARRAY (aty2, _))
                       -> if aty1 = aty2 then ()
                          else error pos "type of array's elements mismatched."

    | (ARRAY _ , _ )   -> error pos "expecting an array."
    | _                -> error pos "expecting an integer, string, array, or record."

// (NAME a, (NAME b, (NAME c, T))) or could be empty
let rec actualTy (tenv, ty: Ty) =
    match ty with
    | NAME (sym, opTy) -> match !opTy with
                          | None    -> match Store.lookup (tenv, sym) with
                                       | None     -> ty    // was a placeholder - fill it
                                       | Some ty' -> opTy := Some ty'
                                                     actualTy (tenv, ty')
                          | Some ty ->  actualTy (tenv, ty)

    | ARRAY (ty', u)   -> ARRAY (actualTy (tenv, ty'), u)
    | _                -> ty

// ______________________________________________________________________________
//

let rec transVar (venv, tenv, (var: Absyn.TVar)) =
    match var with
    | SimpleVar (sym, pos)
                         -> match Store.lookup (venv, sym) with
                            | None   -> error pos (sprintf "undefined variable '%s'." (Store.name sym))
                                        dummyTransExp
                            | Some v -> { exp=(); ty=actualTy (tenv, v.ty) }

    | FieldVar (tVar, sym, pos)
                         -> dummyTransExp

    | SubscriptVar (tVar, e, pos)
                         -> dummyTransExp

and transExp (venv, fenv, tenv, (exp: Absyn.TExp)) =
    match exp with
    | IntExp i           -> dummyTransExp

    | StringExp (str, pos)
                         -> dummyTransExp

    | NilExp             -> dummyTransExp
    | BreakExp pos       -> dummyTransExp
    | VarExp tVar        -> dummyTransExp

    | AssignExp assignRec
                         -> dummyTransExp

    | OpExp opRec        -> let {exp=_; ty=tyleft} = transExp (venv, fenv, tenv, opRec.left)
                            let {exp=_; ty=tyright} = transExp (venv, fenv, tenv, opRec.right)
                            match opRec.oper with
                            | PlusOp   -> checkBothInt (tyleft, tyright, opRec.pos)
                            | MinusOp  -> checkBothInt (tyleft, tyright, opRec.pos)
                            | TimesOp  -> checkBothInt (tyleft, tyright, opRec.pos)
                            | DivideOp -> checkBothInt (tyleft, tyright, opRec.pos)
                            | EqOp     -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            | NeqOp    -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            | GtOp     -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            | GeOp     -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            | LtOp     -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            | LeOp     -> checkBothIntOrString (tyleft, tyright, opRec.pos)
                            {exp=(); ty=Types.INT}

    | CallExp callRec    -> dummyTransExp
    | RecordExp recRec   -> dummyTransExp
    | ArrayExp arrayRec  -> dummyTransExp
    | SeqExp expList     -> dummyTransExp
    | IfExp ifRec        -> dummyTransExp
    | WhileExp whileRec  -> dummyTransExp
    | ForExp forRec      -> dummyTransExp
    | LetExp letRec      -> dummyTransExp

and transDec (venv, tenv, (dec: Absyn.TDec)) =
    match dec with
    | TypeDec typeRecList
                         -> dummyTransExp

    | VarDec varRec      -> dummyTransExp

    | FunctionDec funDecList
                         -> dummyTransExp

and transTy (tenv, (ty: Absyn.TType)) =
    match ty with
    | NameTy (sym, pos)
                         -> dummyTransExp

    | RecordTy fieldRecList
                         -> dummyTransExp

    | ArrayTy (sym, pos)
                         -> dummyTransExp
