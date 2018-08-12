namespace Tiger

open System.IO

open System.Text.RegularExpressions
open Microsoft.FSharp.Text.Lexing

module Lexer =

    let lastToken = Regex @"EOF\s+\d+"

// _____________________________________________________________________________
//                                                              Helper function

    let private tokensToList (lexbuf: LexBuffer<_>) =
        let mutable keepParsing = true
        let mutable tokenList = []

        while keepParsing do
            let token = TigerLex.Read lexbuf

            // Add the token to our list
            tokenList <- tokenList @ [token]

            let last = lastToken.Match token
            if last.Success then
                keepParsing <- false

        tokenList

// _____________________________________________________________________________
//                                                                       Lexing

    let fromStringV (str: string) text =
        if text then
            printf "\n ---------------------------\n %s \n -------------------------------\n" str
        else ()

        LexBuffer<char>.FromString(str)
            |> tokensToList


    let fromString str = fromStringV str false

    let fromFile (filename: string) =
        use reader = new StreamReader(filename)

        LexBuffer<char>.FromTextReader reader
            |> tokensToList
