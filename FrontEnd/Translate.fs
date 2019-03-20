module Translate

open Tree

// ____________________________________________________________________________
//                                                                       Types

// Result type. Tree.Exp is a subset.
type Exp =
    | Ex of Tree.Exp   // value that could be assigned
    | Nx of Tree.Stm   // no value, can't assing
    | Cx of CxFunc     // represents condition

// Function is used to postpone the execution. Needed parameters are known at runtime.
and CxFunc = Temp.Label * Temp.Label -> Tree.Stm

// Main structure
type Level =
    | Top
    | Nested of NestedRec

and NestedRec = { parent: Level; frame: Frame.Frame; uniq: unit ref }

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
    | []      -> EXP (CONST 0)
    | x :: xs -> cons x xs

// To use an IR as an Ex, call this function
let unEx e =
    match e with
    | Ex exp        -> exp
    | Cx genStmFunc -> let r = Temp.newTemp()
                       let t = Temp.newLabel()
                       let f = Temp.newLabel()

                       ESEQ(blockCode [ MOVE (TEMP r, CONST 1); // side effect, preset TEMP r
                                         genStmFunc(t, f);

                                       LABEL f;
                                         MOVE(TEMP r, CONST 0);

                                       LABEL t],
                            TEMP r)                              // result

    | Nx s          -> ESEQ (s, CONST 0) // CONST 0, because s do not return value.
                                         // Could be interpret as false.

//  To use an IR as an Nx, call this function
let unNx e =
    match e with
    | Ex exp        -> EXP exp
    | Cx genStmFunc -> let t = Temp.newLabel()
                       SEQ (genStmFunc(t, t), LABEL t) // call function and go to the end
    | Nx s          -> s

//  To use an IR as an Cx, call this function
let unCx e =
    match e with
    | Ex (CONST 0)  -> fun _ f -> JUMP (NAME f, [f])  // Tip: This two are needed because CJUMP requires two expressions
    | Ex (CONST 1)  -> fun t _ -> JUMP (NAME t, [t])  //      but we have only one
    | Ex exp        -> fun t f -> CJUMP (NE, exp, CONST 0, f, t)
    | Cx genStmFunc -> fun t f -> genStmFunc(t, f)
    | Nx _          -> failwithf "ERROR: Impossible usage of unCx."


type FuncInfo = { parent: Level; name: Temp.Label; formals: bool list }

// When enter a function new level should be set
let newLevel (funInfo: FuncInfo) =
    Nested ({ parent=funInfo.parent; frame=Frame.newFrame{ name=funInfo.name; formalsEsc=true::funInfo.formals }; uniq=ref () })

// "library" functions are declared at this level
let outermost = newLevel({ parent=Top; name=Temp.namedLabel "__main"; formals=[] })

// Return formals associated with the frame in this level,
// excluding the static link (first element of the list p. 127)
let formals (level: Level) = match level with
                             | Top              -> []
                             | Nested nestedRec -> let formalParams = List.tail nestedRec.frame.formals
                                                   List.map (fun x -> (level, x)) formalParams

let allocLocal (level: Level) esc =
    match level with
    | Top              -> failwithf "ERROR: locals can't exist on top level."
    | Nested nestedRec -> (level, Frame.allocLocal nestedRec.frame esc)

// Generate a chain of FP 'MEM(BINOP(PLUS, (MEM(BINOP(PLUS, ... (MEM(BINOP(PLUS, TEMP(Frame.FP), CONST(k))...)'
// of the caller frame to pass as first argument to the callee
let rec genStaticLinkChain (decLevel: Level, usedLevel: Level, currentFP) =
    match (decLevel, usedLevel, currentFP) with
    | (Top, Top, _)  | (Nested _, Top, _) | (Top, Nested _, _) -> failwithf "ERROR: Imposible to generate static link chain."
    | (Nested decLevel, Nested usedLevel, currentFP)           -> if (decLevel.uniq) = (usedLevel.uniq)
                                                                      then currentFP
                                                                      else
                                                                           let staticLinkOffset = Frame.initialOffset
                                                                           // Here 'currentFP' is the FP frome previous frame
                                                                           let curStaticLink = MEM (BINOP (PLUS, CONST(staticLinkOffset), currentFP))

                                                                           genStaticLinkChain(Nested decLevel, usedLevel.parent, curStaticLink)

// ____________________________________________________________________________
//        IR is independent of the details of the source language!

// Contains list of defined procedures or strings
let fragList :Frame.Frag list ref =
    ref []

let newBreakpoint = Temp.newLabel()

// Tip: How to design Translate.fs? Specify how every Tiger language expression should be translated.

// ____________________________________________________________________________
//                                                            transVar section

// Must produce a chain of MEMs as fetch static links between the level of use
// and the level of definition - the level within the variable's access.
let simpleVarIR ((decLevel, access): Access, usedLevel: Level) :Exp =
    Ex (Frame.exp access (genStaticLinkChain(decLevel, usedLevel, TEMP Frame.FP)))

// MEM (BINOP (PLUS, ex1, ex2) as +(ex1, ex2)
let private memplus (ex1:Tree.Exp, ex2:Tree.Exp) = MEM (BINOP (PLUS, ex1, ex2))

// All records and array values are pointers to record and array structures.
// e.g. 'x.all' first we use 'x' address as base (recBase) then find the index of the 'sym'.
let fieldVarIR (recBase, sym, fieldTyList) :Exp =
    // Index depends from the position of the record definition
    let findindex (tyList) = List.findIndex (fun (elm, _) ->
                                                           elm = sym) tyList
    Ex (memplus (unEx recBase, BINOP(MUL, CONST (findindex(fieldTyList)), CONST(Frame.WORDSIZE))))

let subscriptVarIR (arrBase, offset) :Exp =
    Ex (memplus (unEx arrBase, BINOP (MUL, unEx offset, CONST(Frame.WORDSIZE))))

// ____________________________________________________________________________
//                                                            transExp section

let errExp = Ex (CONST 0)

// CONST represents integer
let intIR (n: int) :Exp = Ex (CONST n)

// Try to find same string to reuse it or create new one
let strIR (newString: string) :Exp =
    let sameStr = List.tryFind (fun x -> match x with
                                         | Frame.PROC _                    -> false
                                         | Frame.STRING(_, existingString) -> newString = existingString) !fragList

    match sameStr with
    | Some(Frame.STRING(existingLabel, _)) -> Ex (NAME existingLabel)
    | _                                    -> let newLabel = Temp.newLabel()
                                              fragList := (Frame.STRING(newLabel, newString) :: !fragList)
                                              Ex (NAME newLabel)

// nil is just CONS 0
let nilIR :Exp = Ex (CONST 0)

let breakIR (label) :Exp = Nx (JUMP (NAME label, [label]))

// Assign do not return value
let assignIR (lhs, rhs) :Exp = Nx (MOVE (unEx lhs, unEx rhs))

let binopIR (oper, e1, e2) :Exp =
    let left = unEx e1
    let right = unEx e2

    match oper with
    | Absyn.PlusOp   -> Ex (BINOP (PLUS, left, right))
    | Absyn.MinusOp  -> Ex (BINOP (MINUS, left, right))
    | Absyn.TimesOp  -> Ex (BINOP (MUL, left, right))
    | Absyn.DivideOp -> Ex (BINOP (DIV, left, right))
    | _              -> failwithf "ERROR: binary operator is expected not a relational."

let relopIR (oper, e1, e2) :Exp =
    let left = unEx e1
    let right = unEx e2

    match oper with
    // depends from the result (late binding) so use a function
    | Absyn.EqOp   -> Cx (fun (t, f) -> CJUMP (EQ, left, right, t, f))
    | Absyn.NeqOp  -> Cx (fun (t, f) -> CJUMP (NE, left, right, t, f))
    | Absyn.LtOp   -> Cx (fun (t, f) -> CJUMP (LT, left, right, t, f))
    | Absyn.LeOp   -> Cx (fun (t, f) -> CJUMP (LE, left, right, t, f))
    | Absyn.GtOp   -> Cx (fun (t, f) -> CJUMP (GT, left, right, t, f))
    | Absyn.GeOp   -> Cx (fun (t, f) -> CJUMP (GE, left, right, t, f))
    | _            -> failwithf "ERROR: relational operator is expected not a binary."

let strEQ (str1, str2) = Ex (Frame.externalCall("stringEqual", [unEx str1; unEx str2]))
let strNEQ (str1, str2) = Ex (BINOP(XOR, unEx (strEQ(str1, str2)), CONST(1)))

// Translate a function call f(a1, ..., an) is simple, except that the static
// link must be added as an implicit extra argument p. 166:
//      CALL(NAME l_f, [staticLink, arg_1, ..., arg_n])
let callIR (decLevel, usedLevel, label, args, isProcedure) :Exp =
    match decLevel with
    | Top                                   -> failwithf "ERROR: can't call function from top level."
    | Nested({parent=Top; frame=_; uniq=_}) -> if isProcedure
                                                   then Nx (EXP (Frame.externalCall (Store.name label, List.map unEx args)))
                                                   else Ex (Frame.externalCall (Store.name label, List.map unEx args))

    | _                               -> // Found a difference of static nesting depth between use level and definiton level

                                         // compute static link and add to arg as first arg
                                         let funAddr = genStaticLinkChain(decLevel, usedLevel, TEMP Frame.FP)
                                         let newArgs = funAddr :: (List.map unEx args)

                                         let call = CALL (NAME label, newArgs)

                                         if isProcedure
                                             then Nx (EXP call)
                                             else Ex call

// See the picture on p. 164
let recordIR (fields) :Exp =
    let r = Temp.newTemp()
    let init = MOVE(TEMP r, Frame.externalCall("allocRecord", [CONST(List.length(fields) * Frame.WORDSIZE)]))

    let rec loop (fields, index) =
        match fields with
        | []            -> []
        | first :: rest -> MOVE (memplus(TEMP r, CONST(index * Frame.WORDSIZE)), unEx first) :: loop(rest, index + 1)

    Ex (ESEQ (blockCode(init :: loop(fields, 0)), TEMP r))

let arrayIR (size, init) :Exp =
    Ex (Frame.externalCall("initArray", [unEx size; unEx init]))

// Result is the last exp. Note that the last sequence
// might be a statement, which makes the whole sequence statement.
let sequenceIR (exps: Exp list) :Exp =
    let len = List.length exps

    match exps with
    | []  -> Nx (EXP (CONST 0))  // () is a statement !
    | [e] -> e
    | _   -> let firstN = blockCode(List.map unNx (List.take (len - 1) exps))
             let last = List.last exps

             match last with
             | Nx(s) -> Nx (SEQ (firstN, s))
             | _     -> Ex (ESEQ (firstN, unEx last))

let ifThenIR (test, then') :Exp =
    let t = Temp.newLabel()
    let f = Temp.newLabel()
    let testStmFunc = unCx test

    Nx (blockCode [ testStmFunc t f;
                  LABEL t;
                    unNx then';
                  LABEL f])

// NOTE: we don't need to store the value of the expression, which is computed for
//       it's side effect
let ifThenElseIR (test, thenStm, elseStm) :Exp =
    let t = Temp.newLabel()
    let f = Temp.newLabel()
    let join = Temp.newLabel()

    let testStmFunc = unCx test

    match thenStm, elseStm with
    | Nx thenStm', Nx elseStm' -> Nx (blockCode [ testStmFunc t f;

                                                 LABEL t;
                                                   thenStm';
                                                   JUMP (NAME join, [join]);

                                                 LABEL f;
                                                   elseStm';

                                                 LABEL join])

    | Cx thenStmFunc, Cx elseStmFunc -> let y = Temp.newLabel()
                                        let z = Temp.newLabel()

                                        Cx (fun (t, f) ->  blockCode [ testStmFunc z y;

                                                                     LABEL z;
                                                                       thenStmFunc(t, y);

                                                                     LABEL y;
                                                                       elseStmFunc(t, f)])

    | Cx thenStmFunc, elseStm -> let y = Temp.newLabel()
                                 let z = Temp.newLabel()
                                 let elseExp = unEx elseStm

                                 Cx (fun (t, f) -> blockCode [ testStmFunc z y;

                                                             LABEL z;
                                                               thenStmFunc (t, f);

                                                             LABEL y;
                                                               CJUMP (NE, CONST 0, elseExp, t, f)])

    | thenStm, Cx elseStmFunc -> let y = Temp.newLabel()
                                 let z = Temp.newLabel()
                                 let thenExp = unEx thenStm

                                 Cx (fun (t, f) -> blockCode [ testStmFunc z y;

                                                             LABEL y;
                                                               CJUMP (NE, CONST 0, thenExp, t, f);

                                                             LABEL z;
                                                               elseStmFunc(t, f)])

    | thenStm, elseStm -> let r = Temp.newTemp()
                          let t = Temp.newLabel()
                          let f = Temp.newLabel()

                          let thenExp = unEx thenStm
                          let elseExp = unEx elseStm

                          Ex (ESEQ (blockCode [ MOVE (TEMP r, thenExp);
                                                testStmFunc t f;

                                              LABEL f;
                                                MOVE (TEMP r, elseExp);

                                              LABEL t],
                                    TEMP r))

let whileIR test =
    let testLabel = Temp.newLabel()
    let bodyLabel = Temp.newLabel()
    let doneLabel = Temp.newLabel()

    let testStmFunc = unCx test

    fun body -> let bodyStm = unNx body
                Nx (blockCode [
                              LABEL testLabel;
                                testStmFunc bodyLabel doneLabel;

                              LABEL bodyLabel;
                                bodyStm;
                                JUMP (NAME testLabel, [testLabel]);

                              LABEL doneLabel
                              ])

let forIR (var, lo, hi) =
    let testLabel = Temp.newLabel()
    let bodyLabel = Temp.newLabel()

    let varExp = unEx var
    let loExp = unEx lo
    let hiExp = unEx hi

    let doneLabel = Temp.newLabel()

    fun body -> let bodyStm = unNx body
                Nx (blockCode [ MOVE (varExp, loExp);
                                CJUMP (LE, varExp, hiExp, bodyLabel, doneLabel);

                              LABEL bodyLabel;
                                bodyStm;
                                CJUMP (LT, varExp, hiExp, testLabel, doneLabel);

                              LABEL testLabel;
                                MOVE (varExp, BINOP(PLUS, varExp, CONST 1));
                                JUMP (NAME bodyLabel, [bodyLabel]);

                              LABEL doneLabel])

let letIR (decs, body) :Exp =
    match List.length decs with
    | 0 -> body
    | 1 -> Ex (ESEQ (unNx (List.head decs), unEx body))
    | _ -> let s = List.map unNx decs
           Ex (ESEQ (blockCode s, unEx body))

// Function declaration
// NOTE: Doesn't returns Exp, only cause side effect as changing 'fragList'
let procEntryExit (level: Level, body) =
    let levelFrame =
        match level with
        | Top                                      -> failwithf "ERROR: Function declaration should not happen in top level."
        | Nested({parent=_; frame=frame'; uniq=_}) -> frame'

    let returnStm = MOVE (TEMP Frame.RV, unEx body)
    let bodyStm = Frame.procEntryExit1(levelFrame, returnStm)

    fragList := Frame.PROC({body=bodyStm; frame=levelFrame}) :: !fragList
