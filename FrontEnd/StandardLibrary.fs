module StandardLibrary

open Env
open Types

// Standard Library

let STL = [
            // Prints on standard output.
            // function print(s: string)
            ("print", [STRING], UNIT);

            // Flush the standard output buffer.
            // function flush()
            ("flush", [], UNIT);

            // Read a character from standard input; return empty string on end of file.
            // function getchar() :string
            ("getchar", [], STRING);

            // Give ASCII value of first character of `s`; yields -1 if `s` is empty string.
            // function ord(s: string) :int
            ("ord", [STRING], INT);

            // Single-character string from ASCII value `i`; halt program if `i` out of range.
            // function chr(i: int) :string
            ("chr", [INT], STRING);

            // Number of characters in `s`.
            // function size(s: string) :int
            ("size", [STRING], INT);

            // Substring of string `s`, starting with character first, `n` characters long.
            // Characters are numbered starting at 0.
            // function substring(s: string, first: int, n: int) :string
            ("substring", [STRING; INT; INT], STRING);

            // Concatenation of `s1` and `s2`.
            // function concat (s1: string, s2: string) :string
            ("concat", [STRING; STRING], STRING);

            // Return (i = 0).
            // function not(i : integer) :integer
            ("not", [INT], INT);

            // Terminate execution with code `i`.
            // function exit(i: int)
            ("exit", [INT], UNIT)]

let includes =
    List.map (fun (name, formals, result) ->
              let label = Temp.namedLabel name
              (Store.symbol(name), { level=Translate.newLevel { parent=Translate.outermost;
                                                                name=label;
                                                                formals=(List.map (fun _ -> false) formals) };
                                     label=label;
                                     formals=formals;
                                     result=result}) ) STL
