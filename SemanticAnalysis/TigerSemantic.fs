module TigerSemantic

open Absyn

open Types
open Env
open ErrorMsg

open Translate

// Intermediate language expression and its type (next chapters)
type ExpTy = {exp: Translate.Exp; ty: Types.Ty}

// Variables
type VEnv = Store.Table<VarEntry>

// Functions. How to proceed if extend language with first class functions?
type FEnv = Store.Table<FunEntry>

// Types
type TEnv = Store.Table<Types.Ty>

type ProgEnv = {venv: VEnv; fenv: FEnv; tenv: TEnv}

// Use it to continue analysis
let dummyTransExp = { exp=(); ty=INT }

// ______________________________________________________________________________
//                                                              Helper functions

let checkInt ((ty: Ty), pos) =
    if ty = INT then ()
    else error pos "expected an integer."

let checkString ((ty: Ty), pos) =
    if ty = STRING then ()
    else error pos "expected a string."

let checkBothInt (ty1, ty2, pos) =
    checkInt (ty1, pos)
    checkInt (ty2, pos)

let checkBothIntOrString (ty1, ty2, pos) =
    match (ty1, ty2) with
    | (INT, INT)       -> ()
    | (INT, _)         -> error pos (sprintf "expected an integer but given `%A`" ty2)
    | (STRING, STRING) -> ()
    | (STRING, _)      -> error pos (sprintf "expected a string but given `%A`" ty2)
    | _                -> error pos "expecting an integer or string."

// Tip: NIL is a type and it is equal to every other p. 113

// Ensure both results can be compared by equality
let checkBothEq (ty1, ty2, pos) =
    match (ty1, ty2) with
    | (INT,    _)       -> checkBothIntOrString (ty1, ty2, pos)
    | (STRING, _)       -> checkBothIntOrString (ty1, ty2, pos)

    | (RECORD (fields1, _), RECORD (fields2, _))
                        -> if fields1 = fields2 then ()
                           else error pos "records are not of the same type."

    | (RECORD _ , NIL)  -> ()
    | (NIL, RECORD _)   -> ()
    | (RECORD _, _)     -> error pos "expecting a record."

    | (ARRAY (aty1, _), ARRAY (aty2, _))
                        -> if aty1 = aty2 then ()
                           else error pos "type of array's elements mismatched."

    | (ARRAY _ , _ )    -> error pos "expecting an array."
    | _                 -> error pos "expecting an integer, string, array, or record."

// Ensure the results have the same type
let checkSame (ty1, ty2, pos) =
    match (ty1, ty2) with
    | (NIL, NIL)   -> ()
    | (_, NIL)     -> error pos "expecting nil"
    | (UNIT, UNIT) -> ()
    | (_, UNIT)    -> error pos "expecting unit"
    | (_, NAME _)  -> ()
    | _            -> checkBothEq (ty1, ty2, pos)

let rec checkDup ((decNameList: Store.Symbol list), positions) =
    match (decNameList, positions) with
    | (name::rest, pos::poss) -> if (List.exists (fun x -> name = x) rest)
                                     then error pos (sprintf "duplicated definition `%s`." (Store.name name))
                                     else checkDup(rest, poss)
    | (_, _)                  -> ()

// (NAME a, (NAME b, (NAME c, ref T))), (NAME, nil)  or could be empty
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
//                                                                  Type checker

// Tip: {exp=(); ty=UNIT} could be used as initial value

let rec transVar ((venv: VEnv), fenv, tenv, breakpoint, (var: Absyn.TVar)) :ExpTy =
    // do not forget to return actual type
    match var with
    | SimpleVar (sym, pos)
                         -> printfn "        !SimpleVar"
                            match Store.lookup (venv, sym) with
                            | None   -> error pos (sprintf "undefined variable `%s`." (Store.name sym))
                                        dummyTransExp
                            | Some v -> { exp=(); ty=actualTy (tenv, v.ty) }

    | FieldVar (tVar, sym, pos)
                         -> printfn "    !FieldVar"
                            let varTy = transVar (venv, fenv, tenv, breakpoint, tVar)

                            let recordType = actualTy (tenv, varTy.ty)
                            match recordType with
                            | RECORD (fieldTys, _) -> let rec findField record fieldList =
                                                          match fieldList with
                                                          | []           -> error pos (sprintf "field `%s` is not a member of that record type." (Store.name record))
                                                                            { exp=(); ty=recordType }
                                                          | (s, t)::rest -> if (s = record)
                                                                            then { exp=(); ty=t }
                                                                            else findField record rest

                                                      findField sym fieldTys

                            | _                    -> error pos "expecting a record variable."
                                                      dummyTransExp

    | SubscriptVar (tVar, e, pos)
                         -> printfn "    !SubscriptVar"
                            let init = transExp (venv, fenv, tenv, breakpoint, e)
                            checkInt (actualTy (tenv, init.ty), pos)

                            let subcript = transVar (venv, fenv, tenv, breakpoint, tVar)
                            let arrayTy = actualTy (tenv, subcript.ty)
                            match arrayTy with
                            | ARRAY(ty', _) -> { exp=(); ty=ty' }
                            | _             -> error pos "expecting an array variable."
                                               dummyTransExp

and transExp ((venv: VEnv), (fenv: FEnv), (tenv: TEnv), (breakpoint: BreakPoint), (exp: Absyn.TExp)) :ExpTy =
    match exp with
    | IntExp i           -> printfn "        !IntExp"
                            { exp=(); ty=INT }

    | StringExp (str, pos)
                         -> printfn "        !StringExp"
                            { exp=(); ty=STRING }

    | NilExp             -> printfn "        !NilExp"
                            { exp=(); ty=NIL }

    | BreakExp pos       -> printfn "         !BreakExp"
                            match breakpoint with
                            | Some _ -> ()
                            | None   -> error pos "`break` must be in a `for` or `while` expression"
                            { exp=(); ty=UNIT }

    | VarExp tVar        -> printfn "        !VarExp"
                            transVar (venv, fenv, tenv, breakpoint, tVar)

    | AssignExp assignRec
                         -> printf "         !AssignExp"
                            let variable = transVar (venv, fenv, tenv, breakpoint, assignRec.var)
                            let expression = transExp (venv, fenv, tenv, breakpoint, assignRec.exp)

                            checkSame (actualTy (tenv, variable.ty), actualTy (tenv, expression.ty), assignRec.pos)
                            { exp=(); ty=UNIT }

    | OpExp opRec        -> printf "         !OpExp"
                            let {exp=_; ty=tyleft} = transExp (venv, fenv, tenv, breakpoint, opRec.left)
                            let {exp=_; ty=tyright} = transExp (venv, fenv, tenv, breakpoint, opRec.right)

                            match opRec.oper with
                            | PlusOp | MinusOp | TimesOp | DivideOp    -> checkBothInt (tyleft, tyright, opRec.pos)
                            | EqOp | NeqOp | GtOp | GeOp | LtOp | LeOp -> checkSame (tyleft, tyright, opRec.pos)
                            { exp=(); ty=INT }

    | CallExp callRec    -> printfn "         !CallExp"
                            match Store.lookup (fenv, callRec.func) with
                            | None          -> error callRec.pos (sprintf "undefined function `%s`." (Store.name callRec.func))
                                               { exp=(); ty=NIL }

                            | Some funEntry -> let actualParmL = List.length callRec.args
                                               let formalParmL = List.length funEntry.formals

                                               if actualParmL <> formalParmL
                                                   then error callRec.pos (sprintf "wrong number of arguments. Expecting %i agruments but given %i." formalParmL actualParmL)
                                                        { exp=(); ty=funEntry.result }
                                                   else
                                                        let checkSame (e, formalTy) =
                                                            let expTy = transExp (venv, fenv, tenv, breakpoint, e)
                                                            checkSame (actualTy (tenv, expTy.ty), actualTy (tenv, formalTy), callRec.pos)

                                                        List.map checkSame (List.zip callRec.args funEntry.formals) |> ignore
                                                        // Traslation should be returned in next phase
                                                        { exp=(); ty=funEntry.result }

    | RecordExp recRec   -> printfn "    !RecordExp"
                            match Store.lookup (tenv, recRec.typ) with
                            | None       -> error recRec.pos (sprintf "type `%s` is not defined." (Store.name recRec.typ))
                                            { exp=(); ty=NIL }
                            | Some recTy -> match recTy with
                                            | (RECORD (fieldTys, _)) -> let rectTysL = List.length recRec.fields
                                                                        let fieldTysL = List.length fieldTys
                                                                        if rectTysL <> fieldTysL
                                                                            then
                                                                                 error recRec.pos (sprintf "expecting %i fields but given %i."  fieldTysL rectTysL)
                                                                                 { exp=(); ty=NIL }
                                                                            else
                                                                                 let checkField ((sym, e, p), (tySym, ty)) =
                                                                                     // order of fields must match
                                                                                     if sym = tySym
                                                                                         then
                                                                                             let expTy = transExp (venv, fenv, tenv, breakpoint, e)
                                                                                             checkSame (expTy.ty, ty, p)
                                                                                         else
                                                                                             error p (sprintf "expecting `%s`, given `%s`." (Store.name tySym) (Store.name sym))

                                                                                 List.iter checkField (List.zip recRec.fields fieldTys)
                                                                                 { exp=(); ty=recTy }

                                            | _                      -> error recRec.pos "expecting a record type."
                                                                        { exp=(); ty=NIL }

    | ArrayExp arrayRec  -> printfn "    !ArrayExp"
                            match Store.lookup (tenv, arrayRec.typ) with
                            | None         -> error arrayRec.pos (sprintf "type `%s` is not defined." (Store.name arrayRec.typ))
                                              { exp=(); ty=UNIT }

                            | Some arrayTy -> let size = transExp (venv, fenv, tenv, breakpoint, arrayRec.size)
                                              checkInt (size.ty, arrayRec.pos)

                                              let init = transExp (venv, fenv, tenv, breakpoint, arrayRec.init)
                                              let initTy = ARRAY (actualTy (tenv, init.ty), ref ())   // !! that was tricky
                                              checkSame (initTy, actualTy (tenv, arrayTy), arrayRec.pos)
                                              { exp=(); ty=arrayTy }

    | SeqExp expList     -> printfn "![SeqExp]"
                            match expList with
                            | []          -> { exp=(); ty=UNIT }
                            | lst         -> let runExp initial e =
                                                (transExp (venv, fenv, tenv, breakpoint, (fst e))) :: initial

                                             let result = List.fold runExp [{exp=(); ty=UNIT}] lst
                                             { exp=(); ty=(result.Head.ty) }

    | IfExp ifRec        -> printfn "    !IfExp"
                            let test = transExp (venv, fenv, tenv, breakpoint, ifRec.test)
                            if test.ty = INT then ()
                                else error ifRec.pos "test clause in `if/then/(else)` expression should be an integer."

                            let then' = transExp (venv, fenv, tenv, breakpoint, ifRec.then')

                            match ifRec.else' with
                            | Some e -> let elseExp = transExp (venv, fenv, tenv, breakpoint, e)
                                        checkSame (then'.ty, elseExp.ty, ifRec.pos)
                                        { exp=(); ty=then'.ty }
                            | None _ -> if then'.ty = UNIT then ()
                                            else error ifRec.pos "`if/then` expression does not return a value."
                                        { exp=(); ty=UNIT }

    // 'break' should know that it is in a cycle
    | WhileExp whileRec  -> printfn "    !WhileExp"
                            let test = transExp (venv, fenv, tenv,  breakpoint, whileRec.test)
                            if test.ty = INT then ()
                                else error whileRec.pos "test clause in `while..do` expression should be an integer."

                            let body = transExp (venv, fenv, tenv, newBreakpoint, whileRec.body)
                            if body.ty = UNIT then ()
                                else error whileRec.pos "body of the `while..do` expression does not return a value."
                            { exp=(); ty=UNIT }

    | ForExp forRec      -> printfn "    !ForExp"
                            // let id = transVar (venv, fenv, tenv, forRec.var)
                            let lo = transExp (venv, fenv, tenv, breakpoint, forRec.lo)
                            let hi = transExp (venv, fenv, tenv, breakpoint, forRec.hi)
                            checkBothInt (lo.ty, hi.ty, forRec.pos)

                            // add 'for' id in variable environment
                            let idExpPos = (fst forRec.pos + 4, snd forRec.pos)
                            let id = VarDec { name=forRec.var; escape=forRec.escape;
                                              typ=Some (Store.symbol "int", idExpPos); init=forRec.lo; pos=idExpPos}
                            let {venv=venv'; fenv=_; tenv=_} = transDec (venv, fenv, tenv, breakpoint, id)

                            let body = transExp (venv', fenv, tenv, newBreakpoint, forRec.body)
                            if body.ty = UNIT then ()
                                else error forRec.pos "body of the `for..to..do` expression does not return a value."
                            { exp=(); ty=UNIT }

    | LetExp letRec      -> printfn "!LetExp"
                            let transCurrentDec progEnv dec =
                               transDec (progEnv.venv, progEnv.fenv, progEnv.tenv, breakpoint, dec)

                            let newProgEnv = List.fold transCurrentDec {venv=venv; fenv=fenv; tenv=tenv} letRec.decs
                            transExp (newProgEnv.venv, newProgEnv.fenv, newProgEnv.tenv, breakpoint, letRec.body)

// change environment
and transDec (venv, fenv, tenv, breakpoint, (dec: Absyn.TDec)) :ProgEnv =
    match dec with
    | TypeDec typeRecList
                         -> printfn "   !TypeDec"
                            let addDecl iniTEnv (dec: TypeDecRec) =
                                Store.enter (iniTEnv, dec.name, NAME (dec.name, ref None))  // !!

                            // 1. Enter types heads
                            let tenv' = List.fold addDecl tenv typeRecList

                            let inferDeclTypes iniTEnv' (dec: TypeDecRec) =
                                 Store.enter (iniTEnv', dec.name, transTy (iniTEnv', dec.ty))

                            // 2. Infer types
                            let tenv'' = List.fold inferDeclTypes tenv' typeRecList

                            let rec inCycle (seen, tyRef, pos) =
                                match tyRef with
                                | None   -> error pos "can't find type definition."
                                            true
                                | Some t -> match t with
                                            | NAME (s2, tr) -> if (List.exists (fun x -> x = s2) seen)
                                                                  then true
                                                                  else inCycle (s2::seen, !tr, pos)
                                            | _             -> false

                            let rec checkEach (typeRecList: TypeDecRec list) =
                                match typeRecList with
                                | []       -> ()
                                | f::rest  -> match Store.lookup (tenv'', f.name) with
                                              | Some (NAME (_, r)) -> if (inCycle ([f.name], !r, f.pos))
                                                                          then error f.pos (sprintf "name type `%s` is involved in cyclic defnition." (Store.name f.name))
                                                                          else checkEach (rest)
                                              | _                   -> ()

                            // 3. Check for cyclic NAME definitions
                            checkEach (typeRecList)

                            // 4. Should not have duplicate types
                            let allTypeNames = List.map (fun (x: TypeDecRec) -> x.name) typeRecList
                            let allTypePos =  List.map (fun (x: TypeDecRec) -> x.pos) typeRecList
                            checkDup (allTypeNames, allTypePos)

                            { venv=venv; fenv=fenv; tenv=tenv'' }

    | VarDec varDecRec   -> printf "    !VarDec"
                            let {exp=_; ty=ty'} = transExp (venv, fenv, tenv, breakpoint, varDecRec.init)

                            match varDecRec.typ with
                            | Some (sym, p) -> match Store.lookup (tenv, sym) with
                                               | Some symTy -> checkBothEq(ty', symTy, p)
                                                               let venv' = Store.enter (venv, varDecRec.name, {ty=ty'; access=()})
                                                               { venv=venv'; fenv=fenv; tenv=tenv}

                                               | None _     -> error p (sprintf "undefined type `%s`." (Store.name sym))
                                                               { venv=venv; fenv=fenv; tenv=tenv }

                            | None          -> let venv' = Store.enter (venv, varDecRec.name, {ty=ty'; access=()})
                                               { venv=venv'; fenv=fenv; tenv=tenv }

    | FunctionDec funDecRecList
                         -> printfn "   !!FunctionDec"

                            // Parameter type checking
                            let transParam tenv (fieldRec: FieldRec) :ParamEntry =
                                match Store.lookup (tenv, fieldRec.typ) with
                                | None   -> error fieldRec.pos (sprintf "the type of the parameter `%s` is undefined." (Store.name fieldRec.name))
                                            { name=fieldRec.name; ty=NIL }
                                | Some t -> { name=fieldRec.name; ty=actualTy (tenv, t) }

                            // Process function body
                            let transFun (venv, fenv, tenv, breakpoint, (funDecRec: FunDecRec), (funEntry: FunEntry)) =

                                let getParams = List.map (transParam tenv) funDecRec.param

                                let addParam (venv': VEnv) paramEntry =
                                    Store.enter (venv', paramEntry.name, { ty=paramEntry.ty; access=() })

                                match funDecRec.result with
                                | Some (result, resultPos) -> match Store.lookup (tenv, result) with
                                                              | None          -> error resultPos "result type of the function is undefined."
                                                              | Some resultTy -> let expResult =
                                                                                     transExp ((List.fold addParam venv getParams), fenv, tenv, breakpoint, funDecRec.body)

                                                                                 checkSame (resultTy, expResult.ty, resultPos)
                                                                                 // Traslation(using funEntry) should be returned in next phase

                                | None                     -> transExp ((List.fold addParam venv getParams), fenv, tenv, breakpoint, funDecRec.body) |> ignore
                                                              // Traslation(using funEntry) should be returned in next phase

                            // Return the type (sum of types of the parameters) of the funcition head
                            let functionHeader (tenv, funDecRec) :FunEntry =
                                let paramsTy = List.map (fun p -> (transParam tenv p).ty) funDecRec.param

                                match funDecRec.result with
                                | Some (sym, pos) -> match Store.lookup (tenv, sym) with
                                                     | None          -> error pos "undefined type."
                                                                        { formals=paramsTy; result=UNIT }

                                                     | Some resultTy -> { formals=paramsTy; result=resultTy }

                                | None            -> { formals=paramsTy; result=UNIT }

                            // 1. Add function header first
                            let fenv' =
                                List.fold (fun fenv (funDecRec: FunDecRec)
                                                    -> Store.enter (fenv, funDecRec.name, functionHeader (tenv, funDecRec))) fenv funDecRecList

                            // 2. Then process functions bodies and watchout for duplicate paramter names
                            let checkFunDec (funDecRec: FunDecRec) = let allParmNames = List.map (fun (x: FieldRec) -> x.name) funDecRec.param
                                                                     let allParamPos =  List.map (fun (x: FieldRec) -> x.pos) funDecRec.param
                                                                     checkDup (allParmNames, allParamPos)

                                                                     match Store.lookup (fenv', funDecRec.name) with
                                                                     | None          -> error funDecRec.pos "Tiger Semantic Analysis did not find function head."
                                                                     | Some funEntry -> transFun (venv, fenv', tenv, breakpoint, funDecRec, funEntry)

                            // 3. Should not contain duplicate functions name
                            let allFuncNames = List.map (fun (x: FunDecRec) -> x.name) funDecRecList
                            let allFuncPos = List.map (fun (x: FunDecRec) -> x.pos) funDecRecList
                            checkDup (allFuncNames, allFuncPos)

                            // Start checking function declaration
                            List.iter checkFunDec funDecRecList
                            { venv=venv; fenv=fenv'; tenv=tenv }

and transTy (tenv, (ty: Absyn.TType)) :Ty =
    // do not forget to return actual type
    match ty with
    | NameTy (sym, _)    -> printfn "        !NameTy"
                            match Store.lookup (tenv, sym) with
                            | None         -> NAME (sym, ref None)    // ! If not found - create an empty NAME
                            | Some symType -> symType

    | RecordTy fieldRecList
                         -> printfn "          !RecordTy"
                            let allFieldNames = List.map (fun (x: FieldRec) -> x.name) fieldRecList
                            let allFieldPos = List.map (fun (x: FieldRec) -> x.pos) fieldRecList
                            checkDup (allFieldNames, allFieldPos)

                            let getFieldType (fieldRec: FieldRec) =
                                match Store.lookup (tenv, fieldRec.typ) with
                                | None         -> error fieldRec.pos (sprintf "type `%s` is not defined." (Store.name fieldRec.typ))
                                                  None
                                | Some symType -> Some (fieldRec.name, symType)

                            let fieldsList = List.choose id (List.map getFieldType fieldRecList)
                            RECORD (fieldsList, ref ())

    | ArrayTy (sym, pos)
                         -> printfn "    !ArrayTy"
                            match Store.lookup (tenv, sym) with
                            | None ->          error pos (sprintf "type `%s` is not defined." (Store.name sym))
                                               NIL
                            | Some symType ->  ARRAY (symType, ref ())
