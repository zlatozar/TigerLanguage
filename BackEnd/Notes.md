### Canonical trees

See notest in `Canon.fs`

### Code generation

In an **optimal tiling**, no two _adjecent tiles_ ca be combined into a single tile
of lower cost.

You can look at so called `Memory-use`/`Runtime environment`, how records, procedures
etc. are organized as the data structure which hardware functions work/change. The
together organize Abstract Data Type(ADT).

#### Maximal Munch algorithm

1. Start at the root of the IR tree.
2. Find the largest (maximum number of covered IR tree notes) tile **t** that fits.
3. Record the michine instruction corresponding to **t**.
4. **t** covers the root and (perhaps) several other nodes below the root.
   Tile **t** leaves several subtrees uncovered.
5. Ivoke **Maximum Munch** recursively on all subtrees.
6. Emit the machine instructions recorded in step **3** in order of a
   postorder traversal fo the tiled IR tree

ATTENTION: Order of the clauses in F# code matters. Algorithm works top-down, so
at the end list of instructions should be reversed.

Note that _Maximal Munch_ makes a **local decision** when it selects and places the next.
A more ambitious approach, based on **dynamic programming** techniques, takes a global view
and can produce an **optimal tiling**:
```
In an optimal tiling, the sum of the overall tile costs is minimum
```

### Liveness

Before start calculating liveness we need additional information. First step is
to analyze code and build **flow** graph.

A variable is **live** if the variable is used at some later point in the program
and there is not an intervening assignment to the variable (nobody change it).

A variable is _live_ on an edge if there is a directed path from that
edge to a `use` of the variable that does not go through any `def`. A variable is
_live-in_ at a node if it is live on any of the in-edges of that node; it is _live-out_
at a node if it is live on any of the out-edges of the node.

#### Calculate liveness

1. If a variable is in `use[n]`, then it is **live-in** at node `n`. That is, if a statement
   uses a variable, the variable is live on entry to that statement.

2. If a variable is **live-in** at a node `n`, then it is **live-out** at all nodes `m` in `pred[n]`.

3. If a variable is **live-out** at node `w`, and _not in_ `def[n]`, then the variable is also
   **live-in** at `w`. That is, if someone needs the value of `a` at the end of statement `n`,
   and `n` does not provide that value, then `a`'s value is needed even on entry to `n`.

### Register allocation

#### Building the Interference Graph

**Two temporaries can be allocated to the same register if there is no edge connecting them**.
In other words:
Each node in the inteference graph represents a temporary value; each edge `(t1, t2)` indicates
a pair of temporaries that **cannot** be assigned to the same register.

 Based on the liveness analysis, we know where each variable is needed. However, during
register allocation, we need to answer questions of the specific form: are variables **u**
and **v** live at the same time? (And therefore cannot be assigned to the same register.)
To make this question easier to answer, we create an explicit data structure, an
_interference graph_. An interference graph is an _undirected_ graph that has an edge
between two variables if they are live at the same time, that is, if they **interfere**
with each other.

  The most obvious way to compute the interference graph is to look at the set of live
variables between each statement in the program, and add an edge to the graph for every
pair of variables in the same set. This approach is less than ideal for two
reasons. First, it can be rather expensive because it takes **O(n2)** time to look at
every pair in a set of **n** live variables. Second, there is a special case in which two
variables that are live at the same time do not actually interfere with each other: when
they both contain the same value because we have assigned one to the other.  A better way
to compute the interference graph is to focus on the writes.  That is, for each
instruction, create an edge between the variable being written (def[`A`]) to and all the
_other_ live-out variables. (One should not create self edges.)  Our next concern is to
choose a data structure for representing the interference graph. There are many standard
choices for how to represent a graph: _adjacency matrix, adjacency list_, and _edge set_.
**The right way to choose a data structure is to study the algorithm that uses the**
**data structure, determine what operations need to be performed, and then choose the**
**data structure that provide the most efficient implementations of those operations.**
Often times the choice of data structure can have an effect on the time complexity of the
algorithm, as it does here. Register allocation algorithm needs to ask the graph for all
of its vertices and, given a vertex, it needs to known all of the adjacent vertices. Thus,
the correct choice of graph representation is that of an adjacency list.

_Algorithm:_

1. At any non-`MOVE` instruction that defines a variable `A` (def[A]), where the live-out
   variables are `B1,...,Bj`, add interference edges `(A, B1),...,(A, Bj)`.

2. At a MOVE instruction `A := C` (def[`A`], use[`C`]), where variables `B1,...,Bj` are
   live-out, add interference edges `(A, Bj),...,(A, Bj)` for any `Bi` that is **not** the
   same as `C`.

#### Code notes

- **foldBack** (called also _fold\_right_). Here is the example:

```fsharp
List.fold (fun acc x -> x :: acc) [] [1; 2; 3; 4; 5]
// val it : int list = [5; 4; 3; 2; 1]

List.foldBack (fun x acc -> x :: acc) [1; 2; 3; 4; 5] [];;
// val it : int list = [1; 2; 3; 4; 5]
```
`List.fold` starts with an empty result list and goes forward through the input, adding each element
to the front of the result list; therefore the final result is in the reverse order.

`List.foldBack`, on the other hand, goes backward through the input; so each element newly added to
the front of the result list was itself to the front in the original list. So the final result is the
same list as the original.

In other words `fold` starts from front (first/left) element of given list, `foldBack` starts from the
back (last/right) element.
