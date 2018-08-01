namespace Tiger

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Text.Lexing

module Parser =

    let fromString (str: string) =
        let lexbuf = LexBuffer<char>.FromString(str)
        TigerParse.Main TigerLex.Read lexbuf
        // try
        //   TigerParse.Main TigerLex.Read lexbuf

        // with
        //   | exn -> let pos = lexbuf.EndPos
        //            failwithf "%s near line %d, column %d\n"
        //               (exn.Message) (pos.Line + 1) pos.Column

    let fromFile (filename: string) =
        use reader = new StreamReader(filename)
        printf "\nParsing: %s\n" filename

        let lexbuf = LexBuffer<char>.FromTextReader reader
        TigerParse.Main TigerLex.Read lexbuf
        // try
        //   TigerParse.Main TigerLex.Read lexbuf

        // with
        //   | exn -> let pos = lexbuf.EndPos
        //            failwithf "%s in file %s near line %d, column %d\n"
        //               (exn.Message) filename (pos.Line + 1) pos.Column
