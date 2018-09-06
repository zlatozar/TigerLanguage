module Env

open Types

// Needed in next steps. Dummy for now.
type Access = unit

type VarEntry = { ty: Ty; access: Access }
type FunEntry = { formals: Ty list; result: Ty }

type ParamEntry = { name: Store.Symbol; ty: Ty }

// Initially variable environment is empty
let baseVarEnv :Store.Table<VarEntry> = Store.empty<VarEntry>

// Predefined types - int and string
let baseTyEnv :Store.Table<Ty> =
    Store.enterAll Store.empty<Ty> [(Store.symbol "int", INT);
                                    (Store.symbol "string", STRING)]
