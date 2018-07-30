module PrettyPrint

open Absyn
open Tiger.Parser

// _____________________________________________________________________________
//                                                             Helper functions

let private say s = printf "%s" s
let private sayln s = printfn "%s" s

let rec indent d =
    match d with
    | 0  -> ()
    | i  -> say " "; indent (i - 1)

let opname op =
    match op with
    | PlusOp   -> "PlusOp"
    | MinusOp  -> "MinusOp"
    | TimesOp  -> "TimesOp"
    | DivideOp -> "DivideOp"
    | EqOp     -> "EqOp"
    | NeqOp    -> "NeqOp"
    | LtOp     -> "LtOp"
    | LeOp     -> "LeOp"
    | GtOp     -> "GtOp"
    | GeOp     -> "GeOp"

let rec dolist d f args =
    sayln ""
    match args with
    | []   -> ()
    | [a]  -> f(a, d + 1)
    | a::r -> sayln ""; f (a, d + 1); say ","; dolist d f r

// _____________________________________________________________________________
//                                                                         Main

// Tip: This is an example how to traverse the AST

let rec var (expr, d) =
    match expr with
    | SimpleVar s        -> indent d; say "SimpleVar("; say (Store.name s); say ")"

    | FieldVar(v, s)     -> indent d; sayln "FieldVar(";  var (v, d + 1); sayln ",";
                            indent (d + 1); say (Store.name s); say ")"

    | SubscriptVar(v, e) -> indent d; sayln "SubscriptVar("; var (v, d + 1); sayln ",";
                            exp (e, d + 1); say ")"

and exp (expr, d) =
    match expr with
    | VarExp v         -> indent d; sayln "VarExp("; var (v, d + 1); say ")"
    | NilExp             -> indent d; say "NilExp"
    | IntExp i          -> indent d; say "IntExp("; say (string i); say ")"
    | StringExp s       -> indent d; say "StringExp(\""; say s; say "\")"

    | CallExp callRec   -> indent d; say "CallExp("; say (Store.name callRec.func); say ",[";
                           (dolist d exp callRec.args); say "])"

    | OpExp opRec       -> indent d; say "OpExp("; say (opname opRec.oper); sayln ",";
                           exp (opRec.left, d + 1); sayln ","; exp (opRec.right, d + 1); say ")"

    | RecordExp recRec  -> let printFields ((name, e), d) =
                               indent d; say "("; say (Store.name name);
                               sayln ","; exp (e, d + 1);
                               say ")"
                           indent d; say "RecordExp("; say (Store.name recRec.typ);
                           sayln ",["; dolist d printFields recRec.fields; say "])"

    | SeqExp expList    -> indent d; say "SeqExp[";
                           dolist d exp expList; say "]"

    | AssignExp assignRec  -> indent d; sayln "AssignExp("; var(assignRec.var, d + 1); sayln ",";
                              exp (assignRec.exp, d + 1); say ")"

    | IfExp ifRec       -> indent d; sayln "IfExp("; exp (ifRec.test, d + 1); sayln ",";
                           exp (ifRec.then', d + 1);
                           match ifRec.else' with
                           | Some e -> sayln ","; exp (e, d + 1); say ")"
                           | _      -> ()

    | WhileExp whileRec -> indent d; sayln "WhileExp("; exp (whileRec.test, d + 1); sayln ",";
                           exp (whileRec.body, d + 1); say ")"

    | ForExp forRec     -> indent d; sayln "ForExp("; say (Store.name forRec.var); say ",";
                           say (string (!forRec.escape)); sayln ",";
                           exp (forRec.lo, d + 1); sayln ","; exp (forRec.hi,d + 1); sayln ",";
                           exp (forRec.body, d + 1); say ")"

    | BreakExp          -> indent d; say "BreakExp"

    | LetExp letRec     -> indent d; say "LetExp(["; dolist d dec letRec.decs; sayln "],";
                           exp (letRec.body, d + 1); say")"

    | ArrayExp arrayRec -> indent d; say "ArrayExp("; say (Store.name arrayRec.typ);
                           sayln ","; exp (arrayRec.size, d + 1); sayln ",";
                           exp (arrayRec.init, d + 1); say ")"

and dec ((decl: TDec), d) =
    match decl with
    | FunctionDec l -> let field ((fieldRec: FieldRec), d) =
                           indent d; say "("; say (Store.name fieldRec.name);
                           say ","; say (string !fieldRec.escape);
                           say ","; say (Store.name fieldRec.typ); say ")"

                       let funDec ((funDecRec: FunDecRec), d) =
                           indent d; say "("; say (Store.name funDecRec.name); say ",[";
                           dolist d field funDecRec.param; sayln "],";
                           match funDecRec.result with
                           | None        -> say "None"
                           | Some (s, _) -> say "Some("; say s; say ")";
                                            sayln ","; exp (funDecRec.body, d + 1); say ")"

                       indent d; say "FunctionDec["; dolist d funDec l; say "]"

    | VarDec varRec  -> indent d; say "VarDec("; say (Store.name varRec.name);
                        say ","; say (string (!varRec.escape)); say ",";
                        match varRec.typ with
                        | None       -> say "None"
                        | Some(s, _) -> say "Some("; say s; say ")";
                                        sayln ","; exp (varRec.init, d + 1); say ")"

    | TypeDec typeRecList -> let tdec ((typeRec: TypeRec), d) =
                                 indent d; say"("; say (Store.name typeRec.name);
                                 sayln ","; ty (typeRec.ty, d + 1); say ")"
                             indent d; say "TypeDec["; dolist d tdec typeRecList; say "]"

and ty (typs, d) =
    match typs with
    | NameTy s                -> indent d; say "NameTy("; say (Store.name s); say ")"

    | RecordTy fieldRecList   -> let f ((fieldRec: FieldRec), d) =
                                     indent d; say "("; say (Store.name fieldRec.name);
                                     say ","; say (string !fieldRec.escape); say ",";
                                     say (Store.name fieldRec.typ); say ")"
                                 indent d; say "RecordTy["; dolist d f fieldRecList; say "]"

    | ArrayTy s               -> indent d; say "ArrayTy("; say (Store.name s); say ")"

// _____________________________________________________________________________
//

let displayStringProg (strProgram: string) =
    let expr = fromString strProgram
    exp (expr, 0)

let diplayFileProg (filename: string) =
    let expr = fromFile filename
    exp (expr, 0)
