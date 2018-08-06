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

let empty<'a> = Map.empty<int, 'a>

let lookup ((table: Table<'a>), (symbol: Symbol)) = table.TryFind (snd symbol)

// Functional style

// Tip: If value exist the new value will rewrite it. In this way we do shadowing

let enter ((table: Table<'a>), (symbol: Symbol), value) = Map.add (snd symbol) value table

let enterAll (table: Table<'a>) entries =
    List.fold (fun t e ->  enter (t, fst e, snd e)) table entries

let printEntries table =
    List.map (fun (sym, _) -> printf "%s " (name sym)) table
