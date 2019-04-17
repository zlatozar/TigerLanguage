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

        let mainMethod = { parent=Top; name=Temp.namedLabel "__main"; formals=[] }
        let mainLevel = newLevel mainMethod

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv, baseTyEnv, mainLevel,
                                  Temp.newLabel(), (Tiger.Parser.fromString str))

        // translate to IR
        procEntryExit(mainLevel, exp)
        !fragList

    let transFromFile (filename: string) =
        fragList := []

        let mainMethod = { parent=Top; name=Temp.namedLabel "__main"; formals=[] }
        let mainLevel = newLevel mainMethod

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv, baseTyEnv, mainLevel,
                                  Temp.newLabel(), (Tiger.Parser.fromFile filename))

        // translate to IR
        procEntryExit(mainLevel, exp)
        !fragList
