module StandardLibrary

open Env
open Types

// Standard Library

// Prints on standard output
// function print(s: string)
let print = (Store.symbol "print", { formals=[STRING]; result=UNIT })

// Flush the standard output buffer
// function flush()
let flush = (Store.symbol "flush", { formals=[]; result=UNIT })

// Read a character from standard input; return empty string on end of file.
// function getchar() :string
let getchar = (Store.symbol "getchar", { formals=[]; result=STRING })

// Give ASCII value of first character of `s`; yields -1 if `s` is empty string.
// function ord(s: string) :int
let ord = (Store.symbol "ord", { formals=[STRING]; result=INT })

// Single-character string from ASCII value `i`; halt program if `i` out of range.
// function chr(i: int) :string
let chr = (Store.symbol "chr", { formals=[INT]; result=STRING })

// Number of characters in `s`.
// function size(s: string) :int
let size = (Store.symbol "size", { formals=[STRING]; result=INT })

// Substring of string `s`, starting with character first, `n` characters long.
// Characters are numbered starting at 0.
// function substring(s: string, first: int, n: int) :string
let substring = (Store.symbol "substring", { formals=[STRING; INT; INT]; result=STRING })

// Concatenation of `s1` and `s2`.
// function concat (si: string, s2: string) :string
let concat = (Store.symbol "concat", { formals=[STRING; STRING]; result=STRING })

// Return (i = 0).
// function not(i: integer) :integer
let not = (Store.symbol "_not", { formals=[INT]; result=INT })

// Terminate execution with code i.
// function exit(i: int)
let exit = (Store.symbol "exit", { formals=[INT]; result=UNIT })

let includes = [print; flush; getchar; ord; chr; size; substring; concat; not; exit]
