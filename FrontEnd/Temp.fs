module Temp

// Compiler front end do not use machine addresses.
// 'Temp' use pseudo addresses to organize frames. Later phases will use it
// as a scaffold to place real machine addresses.

type Temp = int

// Map wrapper
type Table<'a> = private Table of Map<int, 'a>

[<RequireQualifiedAccess>]
module Table =
    let empty = Table Map.empty
    let enter (Table table) k v = Table (Map.add k v table)
    let look (Table table) s = Map.tryFind s table
    let lookup (Table table) s = Map.find s table
    let bindings (Table table) = Map.toList table
    let contain (Table table) k = Map.containsKey k table

type Label = Store.Symbol

let newTemp =
    let temp = ref 100
    fun () ->
        temp := !temp + 1
        !temp

let makeString t = string t

let newLabel =
    let labs = ref 0
    fun () ->
        labs := !labs + 1
        Store.symbol (sprintf "L_%d" !labs)

let namedLabel = Store.symbol
let stringOfLabel = Store.name

