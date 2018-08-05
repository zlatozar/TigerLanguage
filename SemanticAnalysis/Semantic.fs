namespace Tiger

module Semantic =

    open Env
    open TigerSemantic

    // ______________________________________________________________________________
    //

    let transFromString (str: string) =
        transExp (baseVarEnv,
                  baseFunEnv,
                  baseTyEnv,
                  (Tiger.Parser.fromString str))

    let transFromFile (filename: string) =
        transExp (baseVarEnv,
                  baseFunEnv,
                  baseTyEnv,
                  (Tiger.Parser.fromFile filename))
