namespace Tiger

module SemanticAnalysis =

    open Absyn

    // Intermediate language expression and its type
    type ExpTy = {exp: Translate.Exp; ty: Types.Ty}

    // variables and functions
    type VEnv = Store.Table<Env.EnvEntry>

    // types
    type TEnv = Store.Table<Types.Ty>

    type ProgEnv = {venv: VEnv; tenv: TEnv}

    // let transVar: venv * tenv * Absyn.var -> expty
    // let transExp: venv * tenv * Absyn.exp -> expty
    // let transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
    // let transTy: tenv * Absyn.ty -> Types.ty

    // Tip: Use Prettyprint.fs as scaffold

    let rec transVar (venv, tenv, (var: Absyn.TVar)) =
        match var with
        | SimpleVar (sym, pos)        -> ()
        | FieldVar (tVar, sym, pos)   -> ()
        | SubscriptVar (tVar, e, pos) -> ()

    and transExp (venv, tenv, (exp: Absyn.TExp)) =
        match exp with
        | IntExp i             -> ()
        | StringExp (str, pos) -> ()
        | NilExp               -> ()
        | BreakExp pos         -> ()
        | VarExp tVar          -> ()

        | AssignExp assignRec  -> ()
        | OpExp opRec       -> ()
        | CallExp callRec   -> ()
        | RecordExp recRec  -> ()
        | ArrayExp arrayRec -> ()
        | SeqExp expList    -> ()
        | IfExp ifRec       -> ()
        | WhileExp whileRec -> ()
        | ForExp forRec     -> ()
        | LetExp letRec     -> ()

    and transDec (venv, tenv, (dec: Absyn.TDec)) =
        match dec with
        | TypeDec typeRecList    -> ()
        | VarDec varRec          -> ()
        | FunctionDec funDecList -> ()

    and transTy (tenv, (ty: Absyn.TType)) =
        match ty with
        | NameTy (sym, pos)      -> ()
        | RecordTy fieldRecList  -> ()
        | ArrayTy (sym, pos)     -> ()

// ______________________________________________________________________________
//

    let transProg (parsedProg: Absyn.TExp) :unit =
        printfn "Semantic analysis phase"
