module Temp

type Temp = int
type Table<'a> = Map<int, 'a>
type Label = Store.Symbol

let temps = ref 100

let newTemp =
    let t = !temps
    temps := t+1
    t
let makeString t = string t

module Format =
      let postInc x = 
          let i = !x
          x := i+1
          i
          
      let labs = ref 0
 
      let newLabel = Store.symbol (sprintf "L_%d" (postInc labs))
      let namedLabel = Store.symbol
