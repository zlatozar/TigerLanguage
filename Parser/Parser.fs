namespace Tiger

open System.IO
open Microsoft.FSharp.Text.Lexing

module Parser =

    let fromString (str: string) =
        let lexbuf = LexBuffer<char>.FromString(str)
        try
            TigerParse.Main TigerLex.Read lexbuf

        with
            | exn -> let pos = lexbuf.EndPos
                     failwithf "[Line:%d, Column:%d] %s\n"
                         (pos.Line + 1) pos.Column exn.Message

    let fromFile (filename: string) =
        use reader = new StreamReader(filename)
        printf "\nParsing: %s\n" filename

        let lexbuf = LexBuffer<char>.FromTextReader reader
        try
            TigerParse.Main TigerLex.Read lexbuf

        with
            | exn -> let pos = lexbuf.EndPos
                     failwithf "[Line:%d, Column:%d] %s\n"
                         (pos.Line + 1) pos.Column exn.Message
