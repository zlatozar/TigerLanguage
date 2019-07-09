module GraphRep

open System.Collections.Generic

type NodeIdx = int

[<NoComparison>]
type NodeRep =
    { succ: NodeIdx list; pred: NodeIdx list }
    with
        static member empty = { succ=[]; pred=[] }
        override this.ToString() =
            sprintf "[succ: %A, pred: %A]" this.succ this.pred

let emptyNode :NodeRep = NodeRep.empty

// array because of index usage
type Graph = ResizeArray<NodeRep>

// Everything is a CONSing
// [<CustomEquality; CustomComparison>]
type Node(g: Graph, i) =
    member __.graph = g
    member __.idx = i

    override __.Equals n2 =
        match n2 with
        | :? Node as n2 -> i = n2.idx
        | _             -> false
    override __.GetHashCode() = hash i
    override __.ToString() = sprintf "node:%i" i // for debugging only

    interface System.IComparable with
        member n1.CompareTo nObj =
            match nObj with
            | :? Node as n2 -> compare n1.idx n2.idx
            | _             ->  failwith "ERROR: Cannot compare values of different types."

[<RequireQualifiedAccess>]
module Graph =

    let newGraph = new ResizeArray<NodeRep>()

    let nodes (g: Graph) :Node list =
        let len = g.Count

        let rec nodes' i =
            if i = len then []
            else Node (g, i) :: nodes' (i + 1)
        nodes' 0

    let private augment g n = Node (g, n)

    let succ (n: Node) :Node list =
        let {succ=succ; pred=_ } = n.graph.Item n.idx
        List.map (augment n.graph) succ

    let pred (n: Node) :Node list =
        let {succ=_; pred=pred } = n.graph.Item n.idx
        List.map (augment n.graph) pred

    let adj gi :Node list =
        (pred gi) @ (succ gi)

    let newNode (g: Graph) :Node =
        g.Add emptyNode
        // graph with node index
        Node (g, (g.Count - 1))

    let private diddleEdge changeFunc (a: Node) (b: Node) :unit =
        let g = a.graph

        let i = a.idx
        let j = b.idx

        let {succ=si; pred=pi} = g.Item i
        let {succ=sj; pred=pj} = g.Item j

        g.[i] <- {succ = (changeFunc j si); pred = pi}
        g.[j] <- {succ = sj; pred = (changeFunc i pj)}

    exception GraphEdge

    let rec private delete i (j: int list) =
        match j with
        | []      -> raise GraphEdge
        | h :: tl -> if i = h
                         then tl
                         else h :: (delete i tl)

    let mkEdge (a: Node) (b: Node) :unit = diddleEdge (fun h t -> h :: t) a b
    let rmEdge (a: Node) (b: Node) :unit = diddleEdge delete a b

    type Table<'a when 'a: comparison> = Table of Map<'a, int>

    module Table =
        let empty = Table Map.empty
        let enter (Table table) k v = Table (Map.add k v table)
        let look (Table table) s = Map.tryFind s table
