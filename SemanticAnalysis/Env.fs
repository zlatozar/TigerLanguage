module Env

open Types

// Needed in next steps. Dummy for now.
type Access = unit

type EnvEntry =
    | VarEntry
    | FunEntry

type VarEntry = { ty: Ty; access: Access }
type FunEntry = { formals: Ty list; result: Ty }

// predefined types
let base_tenv = () // :Store.Table<Ty> = ()

// predefined functions
let base_venv = () // :Store.Table<EnvEntry> = ()
