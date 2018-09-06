module Temp

// Compiler front end do not use machine addresses. 
// 'Temp' use pseudo addresses to organize frames. Later phases will use it
// as a scaffold to place real machine addresses.

type Temp = int
type Table<'a> = Map<int, 'a>
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
