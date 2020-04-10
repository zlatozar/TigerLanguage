module FindEscape

open Absyn

// A variable or formal parameter escapes if its declared in a higher function
// and is used in a lower function.

type depth = int
type escEnv = Store.Table<depth * bool ref>

// look up through the variable references up to underlying var
let rec traverseVar (env: escEnv, d: depth, var: TVar) :unit =
    match var with
    | SimpleVar (sym, _) -> match Store.lookup(env, sym) with
                            | Some(depth, esc)  -> if d > depth then esc := true else ()
                                                   // printf "%s -> declared: %i, usded: %i\n" (Store.name sym) depth d
                            | None              -> ()

    | FieldVar (tVar, _, _)
                         -> traverseVar(env, d, tVar)

    | SubscriptVar (tVar, _, _)
                         -> traverseVar(env, d, tVar)

and traverseExp (env: escEnv, d: depth, exp: TExp) :unit =
    match exp with
    | IntExp _           -> ()
    | StringExp _        -> ()
    | NilExp             -> ()
    | BreakExp _         -> ()

    | VarExp tVar        -> traverseVar(env, d, tVar)

    | AssignExp assignRec
                         -> traverseExp(env, d, assignRec.exp)
                            traverseVar(env, d, assignRec.var)

    | OpExp opRec        -> traverseExp(env, d, opRec.left)
                            traverseExp(env, d, opRec.right)

    | CallExp callRec    -> List.iter (fun e -> traverseExp(env, d, e)) callRec.args

    | RecordExp recRec   -> List.iter (fun (_, e, _) -> traverseExp(env, d, e)) recRec.fields

    | ArrayExp arrayRec  -> traverseExp(env, d, arrayRec.size)
                            traverseExp(env, d, arrayRec.init)

    | SeqExp expList     -> List.iter (fun (e, _) -> traverseExp(env, d, e)) expList

    | IfExp ifRec        -> traverseExp(env, d, ifRec.test)
                            traverseExp(env, d, ifRec.then')
                            match ifRec.else' with
                            | Some(e)  -> traverseExp(env, d, e)
                            | None     -> ()

    | WhileExp whileRec  -> traverseExp(env, d, whileRec.test)
                            traverseExp(env, d, whileRec.body)

    | ForExp forRec      -> let loopEnv = Store.enter(env, forRec.var, (d, forRec.escape))
                            forRec.escape := false

                            traverseExp(env, d, forRec.lo)
                            traverseExp(env, d, forRec.hi)
                            traverseExp(loopEnv, d, forRec.body)

    | LetExp letRec      -> let newEnv = traverseDecs(env, d, letRec.decs)
                            traverseExp(newEnv, d, letRec.body)

and traverseDecs (parentEnv: escEnv, d: depth, decs: TDec list) :escEnv =

    let traversDec env dec =
        match dec with
        | TypeDec _          -> env

        | VarDec varDecRec   -> varDecRec.escape := false
                                traverseExp(env, d, varDecRec.init)
                                Store.enter(env, varDecRec.name, (d, varDecRec.escape))

        | FunctionDec funDecRecList
                            -> let traverseParams beforeEnv funDecRec =
                                   let newEnv = List.fold (fun locEnv fieldRec -> fieldRec.escape := false
                                                                                  Store.enter(locEnv, fieldRec.name, ((d + 1), fieldRec.escape))
                                                          ) beforeEnv funDecRec.param
                                   traverseExp (newEnv, (d + 1), funDecRec.body)
                                   beforeEnv

                               List.fold traverseParams env funDecRecList

    List.fold traversDec parentEnv decs

// Side effect 'programm' as add escapes
let findEscape(program: TExp): unit =
    traverseExp(Store.empty<depth * bool ref>, 0, program)
