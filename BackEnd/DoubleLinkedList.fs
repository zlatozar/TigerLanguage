// A mutable, circular, doubly linked list
module DLList

[<CustomEquality;CustomComparison>]
type NodeType<'a when 'a: comparison> = {
                      mutable data: 'a;
                      mutable next: NodeType<'a>;
                      mutable prev: NodeType<'a>
                    }
    with
        override __.Equals n2 =
            match n2 with
            | :? NodeType<'a> as n2 -> __.data = n2.data
            | _                     -> false
        override __.GetHashCode() = hash __.data

        interface System.IComparable with
            member __.CompareTo nObj =
                match nObj with
                | :? NodeType<'a> as n2 -> compare __.data n2.data
                | _                     -> failwith "ERROR: Cannot compare values of different types."

exception Empty

let create d =
    let rec node = {
                     data = d;
                     next = node;
                     prev = node
                   }
    node

let length node =
    let rec loop cnt n =
        if n = node then
            cnt
        else
            loop (cnt + 1) n.next
    loop 1 node.next

let append d node =
    let nd = {
               data = d;
               next = node.next;
               prev = node
             }
    node.next.prev <- nd  // circular
    node.next      <- nd
    nd

let insert d node =
    let nd = {
                data = d;
                next = node.next;
                prev = node
                }
    node.next.prev <- nd  // circular
    node.next      <- nd

let drop node =
    let next = node.next
    let prev = node.prev

    prev.next <- next;
    next.prev <- prev;
    node.next <- node;
    node.prev <- node;
    next

let splice node1 node2 =
    let next = node1.next
    let prev = node2.prev

    node1.next <- node2
    node2.prev <- node1
    next.prev  <- prev
    prev.next  <- next

let set node data = node.data <- data

let get node = node.data

let next node = node.next

let prev node = node.prev

let foldLeft f init node =
    let rec loop accu n =
        if n = node then
            accu
        else
            loop (f accu n.data) n.next

    loop (f init node.data) node.next

let foldRight f node init =
    let rec loop accu n =
        if n = node then
            f n.data accu
        else
            loop (f n.data accu) n.prev

    loop init node.prev

let map f node =
    let first = create (f node.data)

    let rec loop last n =
        if n = node then
            first.prev <- last
            first
        else
            let nn = {
                       data = f n.data;
                       next = first;
                       prev = last
                     }
            last.next <- nn
            loop nn n.next

    loop first node.next

let iter f node =
    f node.data |> ignore

    let rec loop n =
        if n <> node then
            f n.data |> ignore
            loop n.next

    loop node.next

let toList node = foldRight (fun d l -> d::l) node []

let ofList lst =
    match lst with
    | []     -> raise Empty
    | h :: t ->
                let first = create h

                let rec loop last = function
                    | [] ->
                            last.next  <- first
                            first.prev <- last
                            first
                    | h :: t ->
                            let nn = {
                                        data = h;
                                        next = first;
                                        prev = last
                                    }
                            last.next <- nn
                            loop nn t

                loop first t