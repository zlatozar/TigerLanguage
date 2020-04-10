namespace FrontEnd

open Microsoft.FSharp.Text.Lexing

open Absyn

module LexParse =

    let tryParse (str: string) :Stmt =
        let lexbuf = LexBuffer<char>.FromString(str)
        try
            LinePar.Main LineLex.Token lexbuf

        with
            | exn -> let pos = lexbuf.EndPos
                     failwithf "%s near line %d, column %d\n"
                                    (exn.Message) (pos.Line + 1) pos.Column

    let tryLex (str: string) =
        let mutable keepParsing = true
        let mutable tokenList = []

        let lexbuf = LexBuffer<char>.FromString(str)
        while keepParsing do
            let token = LineLex.Token lexbuf

            // Add the token to our list
            tokenList <- tokenList @ [token]

            if token = LinePar.token.EOF then
                keepParsing <- false

        tokenList
