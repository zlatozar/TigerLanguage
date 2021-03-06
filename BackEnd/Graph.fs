module GraphRep

type NodeIdx = int

[<NoComparison>]
type NodeRep =
    { succ: NodeIdx list; pred: NodeIdx list }
    with
        static member empty = { succ=[]; pred=[] }
        override this.ToString() =
            sprintf "{pred: %A, succ: %A}" this.succ this.pred

let emptyNodeRep :NodeRep = NodeRep.empty

// array because of index usage/iteration
type Graph = ResizeArray<NodeRep>

// [<CustomEquality; CustomComparison>]
type Node(g: Graph, i) =
    // current state of the graph
    member __.graph = g
    // index is need to re-create nodes number
    member __.idx = i

    override __.Equals n2 =
        match n2 with
        | :? Node as n2 -> i = n2.idx
        | _             -> false
    override __.GetHashCode() = hash i
    override __.ToString() = sprintf "node:%i" i

    interface System.IComparable with
        member n1.CompareTo nObj =
            match nObj with
            | :? Node as n2 -> compare n1.idx n2.idx
            | _             -> failwith "ERROR: Cannot compare values of different types."

[<RequireQualifiedAccess>]
module Graph =

    // every time new instance
    let newGraph = fun() -> new ResizeArray<NodeRep>()

    let nodes (g: Graph) :Node list =
        let len = g.Count

        let rec nodes' i =
            if i = len then []
            else Node (g, i) :: nodes' (i + 1)
        nodes' 0

    let private augment g n = Node (g, n)

    let succ (n: Node) :Node list =
        let {succ=succ; pred=_} = n.graph.Item n.idx
        List.map (augment n.graph) succ

    let pred (n: Node) :Node list =
        let {succ=_; pred=pred} = n.graph.Item n.idx
        List.map (augment n.graph) pred

    let adj gi :Node list =
        (pred gi) @ (succ gi)

    // Node([NodeRep0<p,s>; NodeRep1<p,s>], 2)
    let newNode (g: Graph) :Node =
        g.Add emptyNodeRep
        // graph with next array index
        Node (g, (g.Count - 1))

    let eq (n1: Node) (n2: Node) =
        n1 = n2

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

    // when debugging
    let show (g: Graph) :unit =
        printfn "\ngraph -> ["
        (nodes g)
            |> List.iter (fun n -> printf "node:%i{succ:%A, pred:%A}; " n.idx (succ n) (pred n))
        printfn "]\n"

    // In this table we save 'a. 'a is what we need at current state of the Graph(given node)
    type Table<'a> = private Table of Map<Node, 'a>

    module Table =
        let empty = Table Map.empty
        let add (Table table) k v = Table (Map.add k v table)
        let iter f (Table table) = Map.iter f table
        // if not found - compiler error
        let lookup (Table table) s = Map.find s table

    type ITable<'a> = private ITable of Map<Node, 'a>

    module ITable =
        let empty = ITable Map.empty
        let add (ITable table) k v = ITable (Map.add k v table)
        let iter f (ITable table) = Map.iter f table
        let lookup (ITable table) s = Map.find s table
