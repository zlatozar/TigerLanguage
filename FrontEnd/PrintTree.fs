module PrintTree

open Tree

// Use for debugging during development

let private say s = printf "%s" s
let private sayln s = printfn "%s" s

let rec indent d =
    match d with
    | 0  -> ()
    | i  -> say " "; indent (i - 1)

let rec stm(i, d) =
    match i with
    | SEQ(a, b)            -> indent d; sayln "SEQ("; stm(a, d + 1); sayln ","; stm(b, d + 1); say ")"
    | LABEL lab            -> indent d; say "LABEL "; say (Store.name lab)
    | JUMP (e, _)          -> indent d; sayln "JUMP("; exp(e, d + 1); say ")"

    | CJUMP(r, a, b, t, f) -> indent d; say "CJUMP("
                              relop r; sayln ","
                              exp(a, d + 1); sayln ","; exp(b, d + 1); sayln ","
                              indent(d + 1); say(Store.name t);
                              say ","; say (Store.name f); say ")"

    | MOVE(a, b)           -> indent d; sayln "MOVE("; exp(a, d + 1); sayln ","
                              exp(b, d + 1); say ")"

    | EXP e                -> indent d; sayln "EXP("; exp(e, d + 1); say ")"

and exp(i, d) =
    match i with
    | BINOP(p, a, b) -> indent d; say "BINOP("; binop p; sayln ","
                        exp(a, d + 1); sayln ","; exp(b, d + 1); say ")"

    | MEM e          -> indent d; sayln "MEM("; exp(e, d + 1); say ")"
    | TEMP t         -> indent d; say "TEMP t"; say(string t)

    | ESEQ(s, e)     -> indent d; sayln "ESEQ("; stm(s, d + 1); sayln ","
                        exp(e, d + 1); say ")"

    | NAME lab       -> indent d; say "NAME "; say (Store.name lab)
    | CONST i        -> indent d; say "CONST "; say(string i)

    | CALL(e, el)    -> indent d; sayln "CALL("; exp(e, d + 1)
                        List.iter (fun a -> (sayln ","; exp(a, d + 2))) el
                        say ")"

and binop i =
    match i with
    | PLUS    -> say "PLUS"
    | MINUS   -> say "MINUS"
    | MUL     -> say "MUL"
    | DIV     -> say "DIV"
    | AND     -> say "AND"
    | OR      -> say "OR"
    | LSHIFT  -> say "LSHIFT"
    | RSHIFT  -> say "RSHIFT"
    | ARSHIFT -> say "ARSHIFT"
    | XOR     -> say "XOR"

and relop i =
    match i with
    | EQ  -> say "EQ"
    | NE  -> say "NE"
    | LT  -> say "LT"
    | GT  -> say "GT"
    | LE  -> say "LE"
    | GE  -> say "GE"
    | ULT -> say "ULT"
    | ULE -> say "ULE"
    | UGT -> say "UGT"
    | UGE -> say "UGE"

let printTree s =
    stm(s, 0); sayln ""

let printProgram (frag: Frame.Frag list) =
    List.iter (fun f -> match f with
                        | Frame.Frag.PROC proc     -> printTree proc.body
                        | Frame.Frag.STRING (_, s) -> printf "%s" s) frag
