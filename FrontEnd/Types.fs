module Types

open Store

// can be used to compare addresses
type Unique = unit ref

// Tip: Simplify types as much as possible

type Ty =
    | RECORD of (Symbol * Ty) list * Unique
    | NIL
    | INT
    | STRING
    | ARRAY of Ty * Unique
    | NAME of Symbol * Ty option ref  (* used in mutual recursive types and type aliases *)
    | UNIT                            (* for expressions that return no value *)

// When to use NIL and when UNIT? - NIL for Tiger language nil or no type, UNIT for
// everything that do not return value. That's way it is important how parser treats ().
