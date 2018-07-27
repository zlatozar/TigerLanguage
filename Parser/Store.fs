module Store

open System.Collections.Generic

// _____________________________________________________________________________
//                                                              Variables store

type Symbol = string * int

let private nextSym = ref 0

let private vars = new Dictionary<string, int>()

let symbol name =
    let found, value = vars.TryGetValue name
    if (found) then (name, value)
    else let idx = !nextSym
         nextSym := idx + 1
         vars.Add (name, idx)
         (name, idx)

let name (sym, idx) = sym

// _____________________________________________________________________________
//                                                                Generic store
