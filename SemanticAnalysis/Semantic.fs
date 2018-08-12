namespace Tiger

module Semantic =

    open Env
    open TigerSemantic

    // ______________________________________________________________________________
    //

    // Standard Library
    let baseFunEnv :Store.Table<FunEntry> = 
        Store.enterAll Store.empty<FunEntry> StandardLibrary.includes

    let transFromString (str: string) =
        transExp (baseVarEnv,
                  baseFunEnv,
                  baseTyEnv,
                  None,
                  (Tiger.Parser.fromString str))

    let transFromFile (filename: string) =
        transExp (baseVarEnv,
                  baseFunEnv,
                  baseTyEnv,
                  None,
                  (Tiger.Parser.fromFile filename))
