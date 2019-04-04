module Translate

open Tree

// Tip: Here we do not work with variable but with "memory places".
//      That's why when a variable is needed, instructions(Exp type) how to reach it are required.

// ____________________________________________________________________________
//                                                                       Types

// Result type. Tree.Exp is a subset.
type Exp =
    | Ex of Tree.Exp   // value that could be assigned
    | Nx of Tree.Stm   // no value, can't assing
    | Cx of CxFunc     // represents condition, that's why is a function

// Function is used to delay the execution. Needed parameters are known at runtime.
and CxFunc = Temp.Label * Temp.Label -> Tree.Stm

// Main structure
type Level =
    | Top
    | Nested of  NestedRec * unit ref

and NestedRec = { parent: Level; frame: Frame.Frame }

// In this abstraction layer Frame.access is no enough - Level should be considered.
// Level is used to calculate static links.
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
    | []      -> failwith "ERROR: Block code should be not empty."
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

    | Nx s          -> ESEQ (s, CONST 0) // CONST 0, because 's' do not return value.
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

let outermost = Top

type FuncInfo = { parent: Level; name: Temp.Label; formals: bool list }

// When enter a function new level should be set
let newLevel (funInfo: FuncInfo) =
    // 'true': static link, which always escapes
    let formals' = true :: funInfo.formals
    let frame' = Frame.newFrame {name=funInfo.name; formalsEsc=formals'}

    Nested ({ parent=funInfo.parent; frame=frame'}, ref () )

// Return formals associated with the frame in this level,
// excluding the static link (first element of the list p. 127)
let formals (level: Level) = match level with
                             | Top                  -> []
                             | Nested(nestedRec, _) -> let formalParams = List.tail nestedRec.frame.formals
                                                       List.map (fun x -> (level, x)) formalParams

let allocLocal (level: Level) escape =
    match level with
    | Top                   -> failwithf "ERROR: Locals can't exist on top level."
    | Nested (nestedRec, _) -> (level, Frame.allocLocal nestedRec.frame escape)

// ____________________________________________________________________________
//                     IR is independent of the details of the source language

// Contains list of defined procedures or strings
let fragList :Frame.Frag list ref =
    ref []

let newBreakpoint = Temp.newLabel()

// Tip: How to design Translate.fs? Specify how every Tiger language expression should be translated.

// ____________________________________________________________________________
//                                                            transVar section

// Must produce a chain of MEMs as fetch static links between the level of use
// and the level of definition - the level within the variable's access.
let simpleVarIR (access, varUsedLevel) :Exp =
    let (defLevel, defAccess) = access

    match defLevel with
    | Top               -> failwithf "ERROR: Can't pass Top level as current."
    | Nested(_, defRef) -> let rec iter(curLevel, offsetAcc) =
                               match curLevel with
                               | Top                   -> failwithf "ERROR: Failed to find level."
                               | Nested(level, curRef) -> if (defRef = curRef)
                                                              then Frame.exp(defAccess) offsetAcc
                                                              else let staticlink = List.head (Frame.formals level.frame)
                                                                   // MEM(BINOP(PLUS, (MEM(BINOP(PLUS, ... (MEM(BINOP(PLUS, TEMP(Frame.FP), CONST(k))...)
                                                                   iter(level.parent, Frame.exp(staticlink) offsetAcc)

                           // access is defined as offset from the FP p. 156
                           Ex (iter(varUsedLevel, TEMP(Frame.FP)))

// MEM (BINOP (PLUS, ex1, ex2) as +(ex1, ex2)
let private memplus ex1 ex2 = MEM (BINOP (PLUS, ex1, ex2))

// All records and array values are pointers to record and array structures.
// The base address(r) of the array is really the contents of a pointer variable,
// so MEM is required to fetch this base address p. 159
let fieldVarIR (r, sym, fieldList) :Exp =
    // Pre-condition: 'sym' should be member of the record (fieldList) is handled
    // by type checker. We assume that function return index in any cases.
    let findindex list = List.findIndex (fun (elm, _) -> elm = sym) list
    Ex (memplus (unEx r) (BINOP (MUL, CONST (findindex fieldList), CONST Frame.WORDSIZE)))

let subscriptVarIR (a, idx) :Exp =
    let t = Temp.newTemp()
    Ex (ESEQ (blockCode [EXP (Frame.externalCall ("checkArrayBounds", [unEx a; unEx idx]));
                         MOVE (TEMP t, BINOP (PLUS, unEx a, CONST Frame.WORDSIZE))
                        ],
              MEM (BINOP (PLUS, TEMP t, BINOP (MUL, unEx idx, CONST Frame.WORDSIZE)))))

// ____________________________________________________________________________
//                                                            transExp section

let unitExp = Nx (EXP (CONST 0))

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

let breakIR (label) :Exp = Nx (JUMP(NAME label, [label]))

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
    | _              -> failwithf "ERROR: Binary operator is expected not a relational."

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
    | _            -> failwithf "ERROR: Relational operator is expected not a binary."

let strEQ (a, b) =
    let r = Frame.externalCall("stringEqual", [unEx a; unEx b])
    Cx (fun (t, f) -> CJUMP (EQ, r, (CONST 1), t, f))

let strNEQ (a, b) = let genStm = unCx (strEQ (a, b))
                    Cx (fun (t, f) ->  genStm f t)

// Translate a function call f(a1, ..., an) is simple, except that the static
// link must be added as an implicit extra argument p. 166:
//      CALL(NAME l_f, [staticLink, arg_1, ..., arg_n])
let callIR (useLevel, defLevel, label, exps, isProcedure) :Exp =
    match defLevel with
    | Top                              -> failwithf "ERROR: Can't call function from top level."
    | Nested({parent=Top; frame=_}, _) -> if isProcedure
                                              then Nx (EXP (Frame.externalCall(Store.name label, List.map unEx exps)))
                                              else Ex (Frame.externalCall(Store.name label, List.map unEx exps))

    | _                               -> // Find the difference of static nesting depth
                                         // between use level and definiton level
                                         let rec depth level = match level with
                                                               | Top                             -> 0
                                                               | Nested({parent=p; frame=_} , _) -> 1 + depth p

                                         // exclude Top level - so +1
                                         let diff = depth useLevel - depth defLevel + 1

                                         let rec staticLink (d, curLevel) =
                                             if d = 0 then TEMP Frame.FP
                                             else
                                                 match curLevel with
                                                 | Top                 -> failwithf "ERROR: Can't find function definition."
                                                 | Nested(innerRec, _) -> Frame.exp (List.head (Frame.formals innerRec.frame)) (staticLink(d - 1, innerRec.parent))

                                         let call = CALL (NAME label, (staticLink(diff, useLevel)) :: (List.map unEx exps))
                                         if isProcedure
                                             then Nx (EXP call)
                                             else Ex call

// See the picture on p. 164
let recordIR (fields) :Exp =
    let r = Temp.newTemp()
    let len = List.length fields

    let init = MOVE(TEMP r, Frame.externalCall("allocRecord", [CONST (len * Frame.WORDSIZE)]))

    let rec loop (fields, index) =
        match fields with
        | []            -> []
        | first :: rest -> MOVE (memplus (TEMP r) (CONST (index * Frame.WORDSIZE)), unEx first) :: loop(rest, index + 1)

    Ex (ESEQ (blockCode (init :: loop(fields, 0)), TEMP r))

let arrayIR (size, init) :Exp =
    Ex (Frame.externalCall("initArray", [unEx size; unEx init]))

// Result is the last exp. Note that the last sequence
// might be a statement, which makes the whole sequence statement.
let sequenceIR (exps: Exp list) :Exp =
    let len = List.length exps

    match exps with
    | []  -> unitExp  // () is a statement !
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

// NOTE: We don't need to store the value of the expression (Nx), which is computed for
//       it's side effect.
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
    match level with
    | Top                                 -> failwithf "ERROR: Function declaration should not happen in top level."
    | Nested({parent=_; frame=frame'}, _) -> let body' =
                                                 // Procedures do no return values. How to deal with RV?
                                                 Frame.procEntryExit1 (frame', MOVE (TEMP Frame.RV, unEx body))
                                             fragList := Frame.PROC({body=body'; frame=frame'}) :: !fragList
