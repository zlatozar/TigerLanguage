module Env

open Types

// Specifies execution environment

// variable definition
type VarEntry = { access: Translate.Access; ty: Ty }

// function definition
type FunEntry = { level: Translate.Level;
                  label: Temp.Label;
                  formals: Ty list;
                  result: Ty }

// parameter defintion
type ParamEntry = { name: Store.Symbol; ty: Ty; escape: bool ref }

// Initially variable environment is empty
let baseVarEnv :Store.Table<VarEntry> = Store.empty<VarEntry>

// Predefined types are only 'int' and 'string'
let baseTyEnv :Store.Table<Ty> =
    Store.enterAll Store.empty<Ty> [(Store.symbol "int", INT);
                                    (Store.symbol "string", STRING)]
