### Canonical trees

See notest in `Canon.fs`

### Code generation

In an **optimal tiling**, no two _adjecent tiles_ ca be combined into a single tile
of lower cost.

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

