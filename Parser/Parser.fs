namespace Tiger

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Text.Lexing

module Parser =

    let fromString (str: string) =
        let lexbuf = LexBuffer<char>.FromString(str)
        try
          TigerParse.Main (TigerLex.Read lexbuf)

        with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s near line %d, column %d\n"
                      (exn.Message) (pos.Line + 1) pos.Column

    (* Parsing from a file *)

    let fromFile (filename: string) =
        use reader = new StreamReader(filename)
        let lexbuf = LexBuffer<char>.FromTextReader reader
        try
          TigerParse.Main (TigerLex.Read lexbuf)

        with
          | exn -> let pos = lexbuf.EndPos
                   failwithf "%s in file %s near line %d, column %d\n"
                      (exn.Message) filename (pos.Line + 1) pos.Column
