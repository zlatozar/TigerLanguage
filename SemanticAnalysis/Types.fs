module Types

open Store

type Unique = unit ref

// Tip: Simplify types as much as possible

type Ty =
    | RECORD of (Symbol * Ty) list * Unique
    | NIL
    | INT
    | STRING
    | ARRAY of Ty * Unique
    | NAME of Symbol * Ty option ref  (* for forward references *)
    | UNIT                            (* represents no value    *)
