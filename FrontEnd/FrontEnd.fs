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
        // reset |> ignore

        let mainMethod = { parent=Top; name=Temp.namedLabel "__main"; formals=[] }
        let mainLevel = newLevel(mainMethod)

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, mainLevel,
                                  Temp.newLabel, (Tiger.Parser.fromString str))

        // translate to IR
        Translate.procEntryExit(mainLevel, exp)
        Translate.getResult

    let transFromFile (filename: string) =
        // reset |> ignore

        let mainMethod = { parent=Top; name=Temp.namedLabel "__main"; formals=[] }
        let mainLevel = newLevel(mainMethod)

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, mainLevel,
                                  Temp.newLabel, (Tiger.Parser.fromFile filename))

        // translate to IR
        Translate.procEntryExit(mainLevel, exp)
        Translate.getResult
