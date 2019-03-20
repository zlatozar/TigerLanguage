namespace Tiger

module FrontEnd =

    open Env
    open Translate
    open TigerFrontEnd

    // Standard Library
    let private baseFunEnv :Store.Table<FunEntry> =
        Store.enterAll Store.empty<FunEntry> StandardLibrary.includes

// ______________________________________________________________________________
//

    let transFromString (str: string) =
        // clear fragment list and define entry point (main function)
        fragList := []

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, outermost, Temp.newLabel(),
                                  (Tiger.Parser.fromString str))

        // translate to IR
        procEntryExit(outermost, exp)
        !fragList

    let transFromFile (filename: string) =
        fragList := []

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, outermost, Temp.newLabel(),
                                  (Tiger.Parser.fromFile filename))

        // translate to IR
        procEntryExit(outermost, exp)
        !fragList
