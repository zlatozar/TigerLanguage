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

let name (name, (_: int)) = name

let showSymbols = Seq.iter (printf "%A\n") vars

// _____________________________________________________________________________
//                                         Symbol index as a key (optimization)

type Table<'a> = Map<int, 'a>

let empty = Map.empty<int, 'a>

let enter (table: Table<'a>) (symbol: Symbol) value = table.Add ((snd symbol), value)

let lookup (table: Table<'a>) (symbol: Symbol) = table.Item (snd symbol)
