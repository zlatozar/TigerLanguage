module DoubleLinkedListTest

open NUnit.Framework

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Add different elements`` () =
    let lst = DLList.create 1
                    |> DLList.append 2
    DLList.length lst |> should equal 2

[<Fact>]
let ``Add equal elements`` () =
    let lst = DLList.create 1
                    |> DLList.append 2
                    |> DLList.append 3

    DLList.length lst |> should equal 3

[<Fact>]
let ``Convert to list`` () =
    let lst = DLList.create 1

    lst
        |> DLList.append 2
        |> DLList.append 3
        |> ignore

    DLList.toList lst |> should equal [1; 2; 3]

[<Fact>]
let ``Using 'insert'`` () =
    let lst = DLList.create 1
    DLList.append 2 lst |> ignore
    DLList.insert 3 lst

    DLList.toList lst |> should equal [1; 3; 2]

[<Fact>]
let ``Using 'splace'`` () =
    let lst = DLList.create 1

    lst
        |> DLList.append 2
        |> DLList.append 3
        |> ignore

    let lst2 = DLList.create 4

    lst2
        |> DLList.append 5
        |> DLList.append 6
        |> ignore

    // 'prev' because of circularity
    DLList.splice lst.prev lst2
    DLList.toList lst |> should equal [1; 2; 3; 4; 5; 6]

[<Fact>]
let ``Using 'drop'`` () =
    let lst = DLList.create 1

    lst
        |> DLList.append 2
        |> DLList.append 3
        |> ignore

    let newList = DLList.drop lst
    DLList.toList newList |> should equal [2; 3]