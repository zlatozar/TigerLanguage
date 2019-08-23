module Color

open System.Collections.Generic

type Allocation = Temp.Table<Frame.Register>

// Pointer to the list
type Dll<'a when 'a: comparison> = DLList.NodeType<'a> option ref

// Pointer and the element
type DllNode<'a when 'a: comparison> = {
                        mutable node: DLList.NodeType<'a> option;
                        mutable dll: Dll<'a>
                      }

let createDll() = ref None

// Helper functions

let dll2list dll =
    match dll with
    | Some lst -> DLList.toList !lst
    | None     -> []

let moveNode setNode dll =
    let oldDll = setNode.dll

    let head = Option.get !oldDll
    let node = Option.get setNode.node
    let next = DLList.drop node
    
    setNode.dll <- dll

    // Remove from old dll
    if next = node then
        oldDll := None
    else
        if node = head then
            oldDll := Some next
            // Append to new dll
            match !dll with
            | None       -> dll := Some node
            | Some other -> DLList.splice (DLList.prev other) node
        else () // ?? or exception

// Node representation
type CNode = {
               inode: GraphRep.Node;
               n: int;
               setNode: DllNode<CNode>
             }

// 'MOVE' representation
type Move = {
              src: CNode;
              dst: CNode;
              setNode: DllNode<Move>
            }

module CSet =
    type CSet = CSet of Set<CNode>

    let empty = Set.empty<CSet>
    let add = Set.add<CSet>
    let diff = Set.difference<CSet>
    let ofList = Set.ofList<CSet>
    let union = Set.union<CSet>
    let fold = Set.fold
    let elements lst = Set.toList lst |> List.sort

module RSet =
    type RSet = RSet of Set<Frame.Register>

    let empty = Set.empty<RSet>
    let isEmpty = Set.isEmpty<RSet>
    let ofList = Set.ofList<RSet>
    let remove = Set.remove<RSet>
    let choose = Set.minElement
