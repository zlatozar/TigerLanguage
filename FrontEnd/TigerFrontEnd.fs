module TigerFrontEnd

open Absyn

open Types
open Env
open ErrorMsg
open PrettyPrint

// 'exp' holds the intermediate-representation(IR) translation of each Tiger expression
type TreeExp = {exp: Translate.Exp; ty: Types.Ty; name: Store.Symbol option}

// Variables
type VEnv = Store.Table<VarEntry>

// Functions. How to proceed if extend language with first class functions?
type FEnv = Store.Table<FunEntry>

// Types
type TEnv = Store.Table<Types.Ty>

type ProgEnv = {venv: VEnv; fenv: FEnv; tenv: TEnv; exps: Translate.Exp list}

// As marker that 'for' and 'while' should pass. Dummy for now.
type BreakPoint = string option
let newBreakpoint = Some "_break"

// Use it to continue analysis
let errorTransExp = { exp=Translate.errExp; ty=NIL; name=None }

// ______________________________________________________________________________
//                                                              Helper functions

// (NAME a, (NAME b, (NAME c, ref T))), (NAME, nil)  or could be empty
let rec actualTy (ty: Ty, pos: Pos) =
    match ty with
    | NAME (sym, tyRef) -> match !tyRef with
                           | None     -> error pos (sprintf "can't find actual type of `%s`." (Store.name sym))
                                         NIL
                           | Some ty' -> actualTy (ty', pos)

    | ARRAY (t, u)       -> ARRAY (actualTy (t, pos), u)
    | _                  -> ty

let checkInt ((ty: Ty), pos) =
    if ty = INT then ()
    else error pos "expected an integer."

let checkString ((ty: Ty), pos) =
    if ty = STRING then ()
    else error pos "expected a string."

// Tip: When compare types first place expected then the expression type

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

let checkType (t1:Ty, t2:Ty, pos) =
    let ty1 = actualTy (t1, pos)
    let ty2 = actualTy (t2, pos)

    if (ty1 <> ty2) then
        match (ty1, ty2) with
        | (RECORD(_), NIL) -> ()
        | (NIL, RECORD(_)) -> ()
        | _                -> error pos "type mismatched."
    else ()

// Ensure both results can be compared by equality
let checkBothEq (lt, rt, pos) =
    match lt with
    | INT           -> checkType (INT, rt, pos)
    | STRING        -> checkType (STRING, rt, pos)
    | ARRAY(t, u)   -> checkType (ARRAY(t, u), rt, pos)
    | RECORD(fs, u) -> checkType (RECORD(fs, u), rt, pos)
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

let getRecType (tenv, (recTy: Ty), pos) =
    match recTy with
    | NAME (name, _) -> match Store.lookup (tenv, name) with
                        | Some t -> actualTy(t, pos)
                        | _      -> error pos (sprintf
                                        "expecting recursive type. Can't find `%s`." (Store.name name))
                                    NIL
    | _              -> recTy

// ______________________________________________________________________________
//                               Type checker and intermidate representation(IR)

let rec transVar ((venv: VEnv), fenv, tenv, level, breakpoint, (var: Absyn.TVar)) :TreeExp =
    // do not forget to return actual type
    match var with
    | SimpleVar (sym, pos)
                         -> printfn "        !SimpleVar"
                            match Store.lookup (venv, sym) with
                            | None   -> error pos (sprintf "undefined variable `%s`." (Store.name sym))
                                        errorTransExp
                            | Some v -> { exp=Translate.simpleVarIR(v.access, level); ty=actualTy (v.ty, pos); name=None }

    | FieldVar (tVar, sym, pos)
                         -> printfn "    !FieldVar"
                            let variable = transVar (venv, fenv, tenv, level, breakpoint, tVar)

                            match variable.ty with
                            | RECORD (fieldTys, _) -> let rec findField record fieldList =
                                                          match fieldList with
                                                          | []           -> error pos (sprintf "field `%s` is not a member of that record type." (Store.name record))
                                                                            errorTransExp
                                                          | (s, t)::rest -> if (s = record)
                                                                                then { exp=Translate.fieldVarIR(variable.exp, sym, fieldTys);
                                                                                       ty=actualTy (getRecType (tenv, t, pos), pos); name=None }
                                                                                else findField record rest

                                                      findField sym fieldTys

                            | _                    -> error pos "expecting a record variable."
                                                      errorTransExp

    | SubscriptVar (tVar, e, pos)
                         -> printfn "    !SubscriptVar"
                            let index = transExp (venv, fenv, tenv, level, breakpoint, e)
                            checkInt (actualTy (index.ty, pos), pos)

                            let subcript = transVar (venv, fenv, tenv, level, breakpoint, tVar)
                            let arrayTy = actualTy (subcript.ty, pos)

                            match arrayTy with
                            | ARRAY(ty', _) -> { exp=Translate.subscriptVarIR(subcript.exp, index.exp); ty=ty'; name=None }
                            | _             -> error pos "expecting an array variable."
                                               errorTransExp

and transExp ((venv: VEnv), (fenv: FEnv), (tenv: TEnv), level, (breakpoint: BreakPoint), (exp: Absyn.TExp)) :TreeExp =
    match exp with
    | IntExp i           -> printfn "        !IntExp"
                            { exp=Translate.intIR(i); ty=INT; name=None }

    | StringExp (str, pos)
                         -> printfn "        !StringExp"
                            { exp=Translate.strIR(str); ty=STRING; name=None }

    | NilExp             -> printfn "        !NilExp"
                            { exp=Translate.nilIR; ty=NIL; name=None }

    | BreakExp pos       -> printfn "         !BreakExp" // FIXIT
                            match breakpoint with
                            | Some _ -> { exp=Translate.breakIR(); ty=UNIT; name=None}
                            | None   -> error pos "`break` must be in a `for` or `while` expression."
                                        errorTransExp

    | VarExp tVar        -> printfn "        !VarExp"
                            transVar (venv, fenv, tenv, level, breakpoint, tVar)

    | AssignExp assignRec
                         -> printf "         !AssignExp"
                            let variable = transVar (venv, fenv, tenv, level, breakpoint, assignRec.var)
                            let expression = transExp (venv, fenv, tenv, level, breakpoint, assignRec.exp)

                            checkSame (variable.ty, expression.ty, assignRec.pos)
                            { exp=Translate.assignIR(variable.exp, expression.exp); ty=UNIT; name=None }

    | OpExp opRec        -> printf "         !OpExp"
                            let {exp=leftEx; ty=tyleft; name=_ } = transExp (venv, fenv, tenv, level, breakpoint, opRec.left)
                            let {exp=rightEx; ty=tyright; name=_ } = transExp (venv, fenv, tenv, level, breakpoint, opRec.right)

                            match opRec.oper with
                            | PlusOp | MinusOp | TimesOp | DivideOp  -> checkBothInt (tyleft, tyright, opRec.pos)
                                                                        { exp=Translate.binopIR(opRec.oper, leftEx, rightEx); ty=INT; name=None }

                            | EqOp | NeqOp                           -> checkSame (tyleft, tyright, opRec.pos)
                                                                        match tyleft with
                                                                        | STRING -> if (opRec.oper = EqOp)
                                                                                        then { exp=Translate.strEQ(leftEx, rightEx); ty=INT; name=None }
                                                                                        else { exp=Translate.strNEQ(leftEx, rightEx); ty=INT; name=None }
                                                                        | _      -> { exp=Translate.relopIR(opRec.oper, leftEx, rightEx); ty=INT; name=None }

                            | GtOp | GeOp | LtOp | LeOp              -> checkSame (tyleft, tyright, opRec.pos)
                                                                        { exp=Translate.relopIR(opRec.oper, leftEx, rightEx); ty=INT; name=None }


    | CallExp callRec    -> printfn "         !CallExp. %A" callRec.func
                            match Store.lookup (fenv, callRec.func) with
                            | None          -> error callRec.pos (sprintf "undefined function `%s`." (Store.name callRec.func))
                                               errorTransExp

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
                                                        errorTransExp
                                                   else
                                                        let argExps = List.map (fun a -> transExp (venv, fenv, tenv, level, breakpoint, a)) callRec.args
                                                        checkFormals(funEntry.formals, argExps, callRec.pos);

                                                        let isProcedure = funEntry.result = UNIT
                                                        let actualParams = List.map (fun elm -> elm.exp) argExps

                                                        // call the function
                                                        { exp=Translate.callIR(level, funEntry.level, funEntry.label, actualParams, isProcedure);
                                                          ty=actualTy (funEntry.result, callRec.pos); name=None }

    | RecordExp recRec   -> printfn "    !RecordExp. Type: %A" recRec.typ
                            match Store.lookup (tenv, recRec.typ) with
                            | None       -> error recRec.pos (sprintf "record type `%s` is not defined." (Store.name recRec.typ))
                                            errorTransExp

                            | Some recTy -> match actualTy (recTy, recRec.pos) with
                                            | RECORD (fieldTys, u) -> let expList = List.map (fun (_, e, _) -> transExp (venv, fenv, tenv, level, breakpoint, e)) recRec.fields

                                                                      let expTypes = List.map (fun {exp=_; ty=ty; name=_} -> ty) expList
                                                                      let exps = List.map (fun {exp=exp; ty=_; name=_} -> exp) expList

                                                                      let checkRecord (ts, fs, pos) =
                                                                          let lts = List.length ts
                                                                          let lfs = List.length fs
                                                                          if (lts <> lfs) then
                                                                              error pos (sprintf "%i fields needed, but %i given." lts lfs)
                                                                          else
                                                                              List.iter (fun (t, ty) -> checkType(getRecType (tenv, snd t, pos), ty, pos)) (List.zip ts fs)

                                                                      checkRecord(fieldTys, expTypes, recRec.pos)

                                                                      { exp=Translate.recordIR(exps); ty=RECORD(fieldTys, u); name=Some recRec.typ }

                                            | _                    -> error recRec.pos "expecting a record type."
                                                                      errorTransExp

    | ArrayExp arrayRec  -> printfn "    !ArrayExp. Type: %A" arrayRec.typ
                            match Store.lookup (tenv, arrayRec.typ) with
                            | None         -> error arrayRec.pos (sprintf "type `%s` is not defined." (Store.name arrayRec.typ))
                                              errorTransExp

                            | Some arrayTy -> let size = transExp (venv, fenv, tenv, level, breakpoint, arrayRec.size)
                                              checkInt (size.ty, arrayRec.pos)

                                              let arrTy = actualTy(arrayTy, arrayRec.pos)
                                              match arrTy with
                                              | ARRAY(aat, g) -> let {exp=initExp; ty=initTy; name=_} = transExp (venv, fenv, tenv, level, breakpoint, arrayRec.init)

                                                                 checkType(aat, initTy, arrayRec.pos);
                                                                 {exp=Translate.arrayIR(size.exp, initExp); ty=ARRAY(aat, g); name=(Some arrayRec.typ)}

                                              | _             -> error arrayRec.pos "expecting an array type."
                                                                 errorTransExp

    | SeqExp expList     -> printfn "![SeqExp] (check semantics)"
                            let exps = List.map (fun (exp, _) -> (transExp (venv, fenv, tenv, level, breakpoint, exp)).exp) expList
                            let ty' = match exps with
                                      | [] -> UNIT
                                      | _  -> (transExp (venv, fenv, tenv, level, breakpoint, fst (List.last expList))).ty

                            {exp=Translate.sequenceIR(exps); ty=ty'; name=None}

    | IfExp ifRec        -> printfn "    !IfExp"
                            let test = transExp (venv, fenv, tenv, level, breakpoint, ifRec.test)
                            if test.ty = INT then ()
                                else error ifRec.pos "test clause expression should be an integer."

                            let then' = transExp (venv, fenv, tenv, level, breakpoint, ifRec.then')

                            match ifRec.else' with
                            | Some e -> let elseExp = transExp (venv, fenv, tenv, level, breakpoint, e)
                                        checkSame (then'.ty, elseExp.ty, ifRec.pos)
                                        { exp=Translate.ifThenElseIR(test.exp, then'.exp, elseExp.exp); ty=elseExp.ty; name=None }

                            | None _ -> if then'.ty = UNIT then ()
                                            else error ifRec.pos "`if/then` expression does not return a value."
                                        { exp=Translate.ifThenIR(test.exp, then'.exp); ty=UNIT; name=None }

    // 'break' should know that it is in a cycle
    | WhileExp whileRec  -> printfn "    !WhileExp"
                            let test = transExp (venv, fenv, tenv,  level, breakpoint, whileRec.test)
                            if test.ty = INT then ()
                                else error whileRec.pos "test clause in `while..do` expression should be an integer."

                            let body = transExp (venv, fenv, tenv, level, newBreakpoint, whileRec.body)
                            if body.ty = UNIT then ()
                                else error whileRec.pos "body of the `while..do` expression does not return a value."

                            { exp=Translate.whileIR(test.exp) body.exp; ty=UNIT; name=None }

    | ForExp forRec      -> printfn "    !ForExp"
                            let lo = transExp (venv, fenv, tenv, level, breakpoint, forRec.lo)
                            let hi = transExp (venv, fenv, tenv, level, breakpoint, forRec.hi)
                            checkBothInt (lo.ty, hi.ty, forRec.pos)

                            // add 'for' id in variable environment
                            let idExpPos = (fst forRec.pos + 4, snd forRec.pos)
                            let id = VarDec { name=forRec.var; escape=forRec.escape;
                                              typ=Some (Store.symbol "int", idExpPos); init=forRec.lo; pos=idExpPos}
                            let {venv=venv'; fenv=_; tenv=_} = transDec (venv, fenv, tenv, level, breakpoint, id)

                            let body = transExp (venv', fenv, tenv, level, newBreakpoint, forRec.body)
                            if body.ty = UNIT then ()
                                else error forRec.pos "body of the `for..to..do` expression does not return a value."

                            match Store.lookup (venv', forRec.var) with
                            | None   -> error forRec.pos (sprintf "Compiler bug: Can't find '%s' inital variable of `for..to..do` cycle" (Store.name forRec.var))
                                        errorTransExp
                            | Some v -> { exp=Translate.forIR(Translate.simpleVarIR(v.access, level), lo.exp, hi.exp) body.exp; ty=UNIT; name=None }

    | LetExp letRec      -> printfn "!LetExp"
                            let transCurrentDec progEnv dec =
                               let decl = transDec (progEnv.venv, progEnv.fenv, progEnv.tenv, level, breakpoint, dec)
                               {venv=venv; fenv=fenv; tenv=tenv; exps=(progEnv.exps @ decl.exps)}

                            let newProgEnv = List.fold transCurrentDec {venv=venv; fenv=fenv; tenv=tenv; exps=[]} letRec.decs
                            let {exp=bodyExp; ty=bodyTy} = transExp (newProgEnv.venv, newProgEnv.fenv, newProgEnv.tenv, level, breakpoint, letRec.body)

                            { exp=Translate.letIR(newProgEnv.exps, bodyExp); ty=bodyTy; name=None }

// change environment
and transDec (venv, fenv, tenv, level, breakpoint, (dec: Absyn.TDec)) :ProgEnv =
    match dec with
    | TypeDec typeRecList
                         -> printfn "   !TypeDec"
                            let addHeader iniTEnv (dec: TypeDecRec) =
                                Store.enter (iniTEnv, dec.name, NAME (dec.name, ref None))  // !!

                            // 1. Enter types heads
                            let tenv' = List.fold addHeader tenv typeRecList

                            // 2. Enter bodies
                            let addBody iniTEnv' (dec: TypeDecRec) =
                                Store.enter (iniTEnv', dec.name, transTy (iniTEnv', dec.ty))

                            let tenv'' = List.fold addBody tenv' typeRecList

                            let inferDeclTypes iniTEnv'' (dec: TypeDecRec) =
                                match Store.lookup (iniTEnv'', dec.name) with
                                | Some ty -> Store.enter (iniTEnv'', dec.name, NAME (dec.name, ref (Some ty)))
                                | _       -> iniTEnv''

                            // 3. Update
                            let tenv''' = List.fold inferDeclTypes tenv'' typeRecList

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
                                | f::rest  -> match Store.lookup (tenv''', f.name) with
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

                            { venv=venv; fenv=fenv; tenv=tenv'''; exps=[] }

    | VarDec varDecRec   -> printf "    !VarDec"
                            let {exp=varExp; ty=expTy; name=expName } = transExp (venv, fenv, tenv, level, breakpoint, varDecRec.init)

                            let acc = Translate.allocLocal level !varDecRec.escape
                            let var = Translate.simpleVarIR(acc, level)

                            // Is type specified?
                            match varDecRec.typ with
                            | Some (sym, p) -> // Records and arrays are equal if there names are also equal
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

                                                               let venv' = Store.enter (venv, varDecRec.name, {ty=actualTy (expTy, p); access=acc})
                                                               { venv=venv'; fenv=fenv; tenv=tenv; exps=[Translate.assignIR(var, varExp)] }

                                               | None _     -> error p (sprintf "undefined type `%s`." (Store.name sym))
                                                               { venv=venv; fenv=fenv; tenv=tenv; exps=[] }

                            | None          -> if (expTy = NIL) then error varDecRec.pos "can't use nil." else ()

                                               let venv' = Store.enter (venv, varDecRec.name, {ty=expTy; access=acc})
                                               { venv=venv'; fenv=fenv; tenv=tenv; exps=[Translate.assignIR(var, varExp)] }
    // Things to check for a function:
    //   1. No duplicate formal names
    //   2. Result type exists and match
    //   3. Formal type exists and match
    //   4. Body type checks
    | FunctionDec funDecRecList
                         -> printfn "   !!FunctionDec"

                            // Should not contain duplicate functions name
                            let allFuncNames = List.map (fun (x: FunDecRec) -> x.name) funDecRecList
                            let allFuncPos = List.map (fun (x: FunDecRec) -> x.pos) funDecRecList
                            checkDup (allFuncNames, allFuncPos)

                            // Formal parameter name and type
                            let transParam (env: TEnv) (fieldRec: FieldRec) :ParamEntry =
                                match Store.lookup (env, fieldRec.typ) with
                                | None   -> error fieldRec.pos (sprintf "the type of the parameter `%s` is undefined." (Store.name fieldRec.name))
                                            { name=fieldRec.name; ty=NIL; escape=fieldRec.escape }
                                | Some t -> { name=fieldRec.name; ty=actualTy (t, fieldRec.pos); escape=fieldRec.escape }

                            let functionHeader ((env: TEnv), funDecRec) :FunEntry =
                                let paramsTy = List.map (fun p -> (transParam env p).ty) funDecRec.param

                                let newLevel = Translate.newLevel { parent=level; name=funDecRec.name; formals=List.map (fun (p: FieldRec) -> !p.escape) funDecRec.param }

                                match funDecRec.result with
                                | Some (sym, pos) -> match Store.lookup (env, sym) with
                                                     | None          -> error pos (sprintf "type `%s` is not defined." (Store.name sym))
                                                                        { level=newLevel; label=funDecRec.name; formals=paramsTy; result=UNIT }

                                                     | Some resultTy -> { level=newLevel; label=funDecRec.name; formals=paramsTy; result=resultTy }

                                | None            -> { level=newLevel; label=funDecRec.name; formals=paramsTy; result=UNIT }

                            // Add function header first

                            let fenv' =
                                List.fold (fun fenv (funDecRec: FunDecRec)
                                                    -> Store.enter (fenv, funDecRec.name, functionHeader (tenv, funDecRec))) fenv funDecRecList

                            // Then process functions body

                            let transBody (venv, fenv, tenv, breakpoint, (funDecRec: FunDecRec), (funEntry: FunEntry)) =

                                let getParams = List.map (transParam tenv) funDecRec.param

                                let addParam (env: VEnv) (paramEntry: ParamEntry) =
                                    Store.enter (env, paramEntry.name, { ty=paramEntry.ty; access=Translate.allocLocal funEntry.level (!paramEntry.escape) })

                                // result type
                                match funDecRec.result with
                                | Some (result, pos) -> match Store.lookup (tenv, result) with
                                                              | None          -> error pos "result type of the function is undefined."
                                                              | Some resultTy -> let body =
                                                                                     transExp ((List.fold addParam venv getParams), fenv, tenv, level, breakpoint, funDecRec.body)

                                                                                 checkSame (resultTy, body.ty, pos)
                                                                                 Translate.procEntryExit (funEntry.level, body.exp)

                                // procdure should not have return type
                                | None                     -> let body = transExp ((List.fold addParam venv getParams), fenv, tenv, level, breakpoint, funDecRec.body)

                                                              checkSame (body.ty, UNIT, funDecRec.pos)
                                                              Translate.procEntryExit (funEntry.level, body.exp)

                            let checkFunDec (funDecRec: FunDecRec) = let allParmNames = List.map (fun (x: FieldRec) -> x.name) funDecRec.param
                                                                     let allParamPos =  List.map (fun (x: FieldRec) -> x.pos) funDecRec.param
                                                                     checkDup (allParmNames, allParamPos)

                                                                     match Store.lookup (fenv', funDecRec.name) with
                                                                     | None          -> error funDecRec.pos "Tiger Semantic Analysis did not find function head."
                                                                     | Some funEntry -> transBody (venv, fenv', tenv, breakpoint, funDecRec, funEntry)

                            List.iter checkFunDec funDecRecList
                            { venv=venv; fenv=fenv'; tenv=tenv; exps=[] }

and transTy (tenv, (ty: Absyn.TType)) :Ty =
    match ty with
    | NameTy (sym, p)    -> printfn "        !NameTy"
                            match Store.lookup (tenv, sym) with
                            | None         -> error p (sprintf "type `%s` is not defined." (Store.name sym))
                                              NIL
                            | Some symType -> NAME (sym, ref (Some symType))  // Create an alias

    | RecordTy fieldRecList
                         -> printfn "          !RecordTy"
                            let allFieldNames = List.map (fun (x: FieldRec) -> x.name) fieldRecList
                            let allFieldPos = List.map (fun (x: FieldRec) -> x.pos) fieldRecList
                            checkDup (allFieldNames, allFieldPos)

                            let getFieldType (fieldRec: FieldRec) =
                                match Store.lookup (tenv, fieldRec.typ) with
                                | None         -> error fieldRec.pos (sprintf "type `%s` is not defined." (Store.name fieldRec.typ))
                                                  (fieldRec.name, UNIT)
                                | Some symType -> (fieldRec.name, symType)

                            let fieldsList = List.map getFieldType fieldRecList
                            RECORD (fieldsList, ref ())

    | ArrayTy (sym, pos)
                         -> printfn "    !ArrayTy"
                            match Store.lookup (tenv, sym) with
                            | None ->          error pos (sprintf "type `%s` is not defined." (Store.name sym))
                                               NIL
                            | Some symType ->  ARRAY (symType, ref ())
