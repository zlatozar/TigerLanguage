namespace Tiger
open Translate

module FrontEnd =

    open Env
    open TigerFrontEnd

    // Standard Library
    let private baseFunEnv :Store.Table<FunEntry> =
        Store.enterAll Store.empty<FunEntry> StandardLibrary.includes

// ______________________________________________________________________________
//

    let transFromString (str: string) =
        // clear fragment list and define entry point (main function)
        reset |> ignore

        let main = { parent=Top; name=Temp.namedLabel "main"; formals=[] }
        let mainLevel = newLevel(main)

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, mainLevel,
                                  None, (Tiger.Parser.fromString str))

        // translate to IR
        Translate.procEntryExit(mainLevel, exp)
        Translate.getResult

    let transFromFile (filename: string) =
        reset |> ignore

        let main = { parent=Top; name=Temp.namedLabel "main"; formals=[] }
        let mainLevel = newLevel(main)

        let {exp=exp; ty=_} = transExp (baseVarEnv, baseFunEnv,
                                  baseTyEnv, mainLevel,
                                  None, (Tiger.Parser.fromFile filename))

        // translate to IR
        Translate.procEntryExit(mainLevel, exp)
        Translate.getResult

