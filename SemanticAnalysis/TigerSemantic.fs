module TigerSemantic

open Absyn

open Types
open Env
open ErrorMsg

open Translate

// Intermediate language expression and its type (next chapters)
type ExpTy = {exp: Translate.Exp; ty: Types.Ty; name: Store.Symbol option}

// Variables
type VEnv = Store.Table<VarEntry>

// Functions. How to proceed if extend language with first class functions?
type FEnv = Store.Table<FunEntry>

// Types
type TEnv = Store.Table<Types.Ty>

type ProgEnv = {venv: VEnv; fenv: FEnv; tenv: TEnv}

// Use it to continue analysis
let dummyTransExp = { exp=(); ty=INT; name=None }

// ______________________________________________________________________________
//                                                              Helper functions

// (NAME a, (NAME b, (NAME c, ref T))), (NAME, nil)  or could be empty
let rec actualTy (ty: Ty, pos: Pos) =
    match ty with
    | NAME (sym, tyRef) -> match !tyRef with
                           | None    -> error pos (sprintf "undefined type `%s`." (Store.name sym))
                                        NIL
                           | Some ty -> actualTy (ty, pos)

    | ARRAY (t, u)       -> ARRAY (actualTy (t, pos), u)
    | _                  -> ty

let checkInt ((ty: Ty), pos) =
    if ty = INT then ()
    else error pos "expected an integer."

let checkString ((ty: Ty), pos) =
    if ty = STRING then ()
    else error pos "expected a string."

// ATTENTION: When compare types first plase expected then the expression type

let checkBothInt (ty1, ty2, pos) =
    checkInt (ty1, pos)
    checkInt (ty2, pos)

let checkBothIntOrString (ty1, ty2, pos) =
    match (ty1, ty2) with
    | (INT, INT)       -> ()
    | (INT, _)         -> error pos (sprintf "expected an integer but given `%A`." ty2)
    | (STRING, STRING) -> ()
    | (STRING, _)      -> error pos (sprintf "expected a string but given `%A`." ty2)
    | _                -> error pos "expecting an integer or string."

// Tip: NIL is a type and it is equal to every other p. 113

(* let checkType (t1:Ty, t2:Ty, pos) =
    let t = actualTy (t1, pos)
    if (t <> t2) then              // do not work when NAME ("someName", ref RECORD) here, since it might be recursive

        match (t, t2) with
        | (RECORD(_), NIL) -> ()
        | (NIL, RECORD(_)) -> ()
        | _                -> error pos "type mismatched."
    else () *)

let checkType (t1:Ty, t2:Ty, pos) =
    let t = actualTy (t1, pos)
    match (t, t2) with
    | (RECORD(_), RECORD (_)) -> () // HACK
    | (RECORD(_), NIL)        -> ()
    | (NIL, RECORD(_))        -> ()
    | (_, b)                  -> if (t <> b) then error pos "type mismatched."
                                 else ()
    
// Ensure both results can be compared by equality
let checkBothEq (lt, rt, pos) =
    match lt with
    | INT           -> checkType(INT, rt, pos)
    | STRING        -> checkType(STRING, rt, pos)
    | ARRAY(t, u)   -> checkType(ARRAY(t, u), rt, pos)
    | RECORD(fs, u) -> checkType(RECORD(fs, u), rt, pos)
    | _             -> error pos "can only check equality on int, string, array or record types."

let checkSame (lt, rt, pos) =
    let t = actualTy (lt, pos)
    match (t, rt) with
    | (NIL, NIL)   -> ()
    | (UNIT, UNIT) -> ()
    | (_, UNIT)    -> error pos "expecting unit."
    | _            -> checkBothEq (t, rt, pos)

let rec checkDup ((decNameList: Store.Symbol list), positions) =
    match (decNameList, positions) with
    | (name::rest, pos::poss) -> if (List.exists (fun x -> name = x) rest)
                                     then error pos (sprintf "duplicated definition `%s`." (Store.name name))
                                     else checkDup(rest, poss)
    | (_, _)                  -> ()

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
                            | Some v -> { exp=(); ty=actualTy (v.ty, pos); name=None }

    | FieldVar (tVar, sym, pos)
                         -> printfn "    !FieldVar"
                            let varTy = transVar (venv, fenv, tenv, breakpoint, tVar)

                            match varTy.ty with
                            | RECORD (fieldTys, _) -> let rec findField record fieldList =
                                                          match fieldList with
                                                          | []           -> error pos (sprintf "field `%s` is not a member of that record type." (Store.name record))
                                                                            { exp=(); ty=NIL; name=None }
                                                          | (s, t)::rest -> if (s = record)
                                                                            then { exp=(); ty=actualTy (t, pos); name=None }
                                                                            else findField record rest

                                                      findField sym fieldTys

                            | _                    -> error pos "expecting a record variable."
                                                      dummyTransExp

    | SubscriptVar (tVar, e, pos)
                         -> printfn "    !SubscriptVar"
                            let init = transExp (venv, fenv, tenv, breakpoint, e)
                            checkInt (actualTy (init.ty, pos), pos)

                            let subcript = transVar (venv, fenv, tenv, breakpoint, tVar)
                            let arrayTy = actualTy (subcript.ty, pos)

                            match arrayTy with
                            | ARRAY(ty', _) -> { exp=(); ty=ty'; name=None }
                            | _             -> error pos "expecting an array variable."
                                               dummyTransExp

and transExp ((venv: VEnv), (fenv: FEnv), (tenv: TEnv), (breakpoint: BreakPoint), (exp: Absyn.TExp)) :ExpTy =
    match exp with
    | IntExp i           -> printfn "        !IntExp"
                            { exp=(); ty=INT; name=None }

    | StringExp (str, pos)
                         -> printfn "        !StringExp"
                            { exp=(); ty=STRING; name=None }

    | NilExp             -> printfn "        !NilExp"
                            { exp=(); ty=NIL; name=None }

    | BreakExp pos       -> printfn "         !BreakExp"
                            match breakpoint with
                            | Some _ -> ()
                            | None   -> error pos "`break` must be in a `for` or `while` expression."
                            { exp=(); ty=UNIT; name=None }

    | VarExp tVar        -> printfn "        !VarExp"
                            transVar (venv, fenv, tenv, breakpoint, tVar)

    | AssignExp assignRec
                         -> printf "         !AssignExp"
                            let variable = transVar (venv, fenv, tenv, breakpoint, assignRec.var)
                            let expression = transExp (venv, fenv, tenv, breakpoint, assignRec.exp)

                            checkSame (actualTy (variable.ty, assignRec.pos),
                                       actualTy (expression.ty, assignRec.pos), assignRec.pos)
                            { exp=(); ty=UNIT; name=None }

    | OpExp opRec        -> printf "         !OpExp"
                            let {exp=_; ty=tyleft; name=_ } = transExp (venv, fenv, tenv, breakpoint, opRec.left)
                            let {exp=_; ty=tyright; name=_ } = transExp (venv, fenv, tenv, breakpoint, opRec.right)

                            match opRec.oper with
                            | PlusOp | MinusOp | TimesOp | DivideOp  -> checkBothInt (tyleft, tyright, opRec.pos)
                            | EqOp | NeqOp                           -> checkSame (tyleft, tyright, opRec.pos)
                            | GtOp | GeOp | LtOp | LeOp              -> checkSame (tyleft, tyright, opRec.pos)
                            { exp=(); ty=INT; name=None }
                            
    | CallExp callRec    -> printfn "         !CallExp. %A" callRec.func
                            match Store.lookup (fenv, callRec.func) with
                            | None          -> error callRec.pos (sprintf "undefined function `%s`." (Store.name callRec.func))
                                               { exp=(); ty=NIL; name=None }

                            | Some funEntry -> let actualParmL = List.length callRec.args
                                               let formalParmL = List.length funEntry.formals

                                               let checkFormals (ts, es, pos) =
                                                   let le = List.length es
                                                   let lt = List.length ts
                                                   
                                                   if (lt <> le) then
                                                       error pos (sprintf "%i args needed, but %i given." lt le)
                                                   else 
                                                       List.iter (fun (t, e) -> checkType(t, e.ty, pos)) (List.zip ts es)

                                               if actualParmL <> formalParmL
                                                   then error callRec.pos (sprintf "wrong number of arguments. Expecting %i agruments but given %i." formalParmL actualParmL)
                                                        { exp=(); ty=funEntry.result; name=None }
                                                   else
                                                        let argExps = List.map (fun a -> transExp (venv, fenv, tenv, breakpoint, a)) callRec.args
                                                        checkFormals(funEntry.formals, argExps, callRec.pos);
                                                        { exp=(); ty=actualTy (funEntry.result, callRec.pos); name=None }

    | RecordExp recRec   -> printfn "    !RecordExp. Type: %A" recRec.typ
                            match Store.lookup (tenv, recRec.typ) with
                            | None       -> error recRec.pos (sprintf "record type `%s` is not defined." (Store.name recRec.typ))
                                            { exp=(); ty=NIL; name=None }
                            | Some recTy -> match actualTy (recTy, recRec.pos) with
                                            | RECORD (fieldTys, u) -> let expList = List.map (fun (_, e, _) -> transExp (venv, fenv, tenv, breakpoint, e)) recRec.fields
                                                                      let expTypes = List.map (fun {exp=_; ty=ty; name=_} -> ty) expList
                                                                      let fes = List.map (fun {exp=exp; ty=_; name=_} -> exp) expList
                                                                      
                                                                      let checkRecord (ts, fs, pos) =
                                                                          let lts = List.length ts
                                                                          let lfs = List.length fs
                                                                          if (lts <> lfs) then
                                                                              error pos (sprintf "%i fields needed, but %i given." lts lfs)
                                                                          else
                                                                              List.iter (fun (t, ty) -> checkType(snd t, ty, pos)) (List.zip ts fs)

                                                                      checkRecord(fieldTys, expTypes, recRec.pos)

                                                                      { exp=(); ty=RECORD(fieldTys, u); name=Some recRec.typ }
                                            
                                            | _                    -> error recRec.pos "expecting a record type."
                                                                      { exp=(); ty=NIL; name=None }

    | ArrayExp arrayRec  -> printfn "    !ArrayExp. Type: %A" arrayRec.typ
                            match Store.lookup (tenv, arrayRec.typ) with
                            | None         -> error arrayRec.pos (sprintf "type `%s` is not defined." (Store.name arrayRec.typ))
                                              { exp=(); ty=UNIT; name=None }

                            | Some arrayTy -> let size = transExp (venv, fenv, tenv, breakpoint, arrayRec.size)
                                              checkInt (size.ty, arrayRec.pos)

                                              let arrTy = actualTy(arrayTy, arrayRec.pos)
                                              match arrTy with
                                              | ARRAY(aat, g) -> let {exp=_; ty=initTy; name=_} = transExp (venv, fenv, tenv, breakpoint, arrayRec.init)
                                                                 
                                                                 checkType(aat, initTy, arrayRec.pos);
                                                                 {exp=(); ty=ARRAY(aat, g); name=Some arrayRec.typ}

                                              | _             -> error arrayRec.pos "expecting an array type."
                                                                 {exp=(); ty=NIL; name=None}

    | SeqExp expList     -> printfn "![SeqExp]"
                            match expList with
                            | []          -> { exp=(); ty=UNIT; name=None }
                            | lst         -> let runExp initial e =
                                                (transExp (venv, fenv, tenv, breakpoint, (fst e))) :: initial

                                             let result = List.fold runExp [{exp=(); ty=UNIT; name=None}] lst
                                             { exp=(); ty=(result.Head.ty); name=None }

    | IfExp ifRec        -> printfn "    !IfExp"
                            let test = transExp (venv, fenv, tenv, breakpoint, ifRec.test)
                            if test.ty = INT then ()
                                else error ifRec.pos "test clause expression should be an integer."

                            let then' = transExp (venv, fenv, tenv, breakpoint, ifRec.then')

                            match ifRec.else' with
                            | Some e -> let elseExp = transExp (venv, fenv, tenv, breakpoint, e)
                                        checkSame (then'.ty, elseExp.ty, ifRec.pos)
                                        { exp=(); ty=elseExp.ty; name=None }
                            | None _ -> if then'.ty = UNIT then ()
                                            else error ifRec.pos "`if/then` expression does not return a value."
                                        { exp=(); ty=UNIT; name=None }

    // 'break' should know that it is in a cycle
    | WhileExp whileRec  -> printfn "    !WhileExp"
                            let test = transExp (venv, fenv, tenv,  breakpoint, whileRec.test)
                            if test.ty = INT then ()
                                else error whileRec.pos "test clause in `while..do` expression should be an integer."

                            let body = transExp (venv, fenv, tenv, newBreakpoint, whileRec.body)
                            if body.ty = UNIT then ()
                                else error whileRec.pos "body of the `while..do` expression does not return a value."
                            { exp=(); ty=UNIT; name=None }

    | ForExp forRec      -> printfn "    !ForExp"
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
                            { exp=(); ty=UNIT; name=None }

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
                                match Store.lookup (iniTEnv', dec.name) with
                                | Some ty -> match ty with
                                             | NAME (_, opTy) -> match !opTy with
                                                                 | None   -> opTy := Some (transTy (iniTEnv', dec.ty))
                                                                             iniTEnv'
                                                                 | Some _ -> iniTEnv'
                                             | _              -> iniTEnv'
                                | _       -> iniTEnv'

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
                            let {exp=_; ty=expTy; name=expName } = transExp (venv, fenv, tenv, breakpoint, varDecRec.init)

                            // Is type specified?
                            match varDecRec.typ with
                            | Some (sym, p) -> // NOTE: Records and arrays are equal if there names are also equal

                                               let areAlias (ty1, ty2, pos) =
                                                   let rec actualAlias (ty, p, n) =
                                                       match ty with
                                                       | NAME (s, tyRef) -> match !tyRef with
                                                                            | None    -> error p (sprintf "undefined type `%s`." (Store.name s))
                                                                                         Store.name s
                                                                            | Some ty -> actualAlias (ty, p, Store.name s)
                                                       | _               -> n

                                                   actualAlias (ty1, pos, "") = actualAlias (ty2, pos, " ")

                                               let checkNames (varTy) =
                                                   match expName with 
                                                   | Some name -> if (sym = name) then ()
                                                                  else match Store.lookup (tenv, name) with
                                                                       | Some t -> if areAlias (varTy, t, p) then ()
                                                                                   else 
                                                                                       error varDecRec.pos (sprintf "`%s` is not an alias of `%s`.
                                                                                                                            " (Store.name name) (Store.name sym))
                                                                       | _ -> error varDecRec.pos (sprintf "`%s` is not defined." (Store.name name))
                                                   | None      -> ()
                                               
                                               match Store.lookup (tenv, sym) with
                                               | Some varTy -> checkNames (varTy)
                                                               checkType (varTy, expTy, p)

                                                               let venv' = Store.enter (venv, varDecRec.name, {ty=actualTy (expTy, p); access=()})
                                                               { venv=venv'; fenv=fenv; tenv=tenv}

                                               | None _     -> error p (sprintf "undefined type `%s`." (Store.name sym))
                                                               { venv=venv; fenv=fenv; tenv=tenv }

                            | None          -> if (expTy = NIL) then error varDecRec.pos "can't use nil." else ()

                                               let venv' = Store.enter (venv, varDecRec.name, {ty=expTy; access=()})
                                               { venv=venv'; fenv=fenv; tenv=tenv }

    | FunctionDec funDecRecList
                         -> printfn "   !!FunctionDec"

                            // Parameter type checking
                            let transParam tenv (fieldRec: FieldRec) :ParamEntry =
                                match Store.lookup (tenv, fieldRec.typ) with
                                | None   -> error fieldRec.pos (sprintf "the type of the parameter `%s` is undefined." (Store.name fieldRec.name))
                                            { name=fieldRec.name; ty=NIL }
                                | Some t -> { name=fieldRec.name; ty=actualTy (t, fieldRec.pos) }

                            // Process function body
                            let transFun (venv, fenv, tenv, breakpoint, (funDecRec: FunDecRec), (funEntry: FunEntry)) =

                                let getParams = List.map (transParam tenv) funDecRec.param

                                let addParam (venv': VEnv) (paramEntry: ParamEntry) =
                                    Store.enter (venv', paramEntry.name, { ty=paramEntry.ty; access=() })

                                match funDecRec.result with
                                | Some (result, resultPos) -> match Store.lookup (tenv, result) with
                                                              | None          -> error resultPos "result type of the function is undefined."
                                                              | Some resultTy -> let expResult =
                                                                                     transExp ((List.fold addParam venv getParams), fenv, tenv, breakpoint, funDecRec.body)

                                                                                 checkSame (resultTy, expResult.ty, resultPos)
                                                                                 // Traslation(using funEntry) should be returned in next phase

                                | None                     -> let result = transExp ((List.fold addParam venv getParams), fenv, tenv, breakpoint, funDecRec.body)
                                                              checkSame (result.ty, UNIT, funDecRec.pos)
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
    match ty with
    | NameTy (sym, _)    -> printfn "        !NameTy"
                            match Store.lookup (tenv, sym) with
                            | None         -> NAME (sym, ref None) // If not found - create an empty NAME
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

                            // Take only fields with known type
                            let fieldsList = List.choose id (List.map getFieldType fieldRecList)
                            RECORD (fieldsList, ref ())

    | ArrayTy (sym, pos)
                         -> printfn "    !ArrayTy"
                            match Store.lookup (tenv, sym) with
                            | None ->          error pos (sprintf "type `%s` is not defined." (Store.name sym))
                                               NIL
                            | Some symType ->  ARRAY (symType, ref ())
