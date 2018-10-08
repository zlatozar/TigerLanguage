module Temp

// Compiler frontend do not use machine addresses.
// 'Temp' use pseudo addresses to organize frames. Later phases will use it
// as a scaffold to place real machine addresses.

type Temp = int

type Table<'a> = private Table of Map<int, 'a>

[<RequireQualifiedAccess>]
module Table =
    let empty = Table Map.empty
    let enter (Table table) k v = Table (Map.add k v table)
    let look (Table table) s = Map.tryFind s table

type Label = Store.Symbol

let private temps = ref 100

let newTemp =
    let t = !temps
    temps := t + 1
    t
let makeString t = string t

let private postInc x =
    let i = !x
    x := i + 1
    i

let private labs = ref 0
let newLabel = Store.symbol (sprintf "L_%d" (postInc labs))
let namedLabel = Store.symbol