module Exercises

// _____________________________________________________________________________
//                                                                    Structure

// Form a binary tree as CONSing Notes
type BinTree<'a> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

let empty = Leaf

// _____________________________________________________________________________
//                                                                    Functions

// Tip: You can't insert immediate, you have to search for 'empty'
//      somewhere in the left or right

// Insert a node in a given tree. Duplicates are forbidden.
let rec insert x tree =
    match tree with
    | Leaf                           -> Node(empty, x, empty)
    | Node(treeL, k, treeR) when x<k -> Node(insert x treeL, k, treeR)
    | Node(treeL, k, treeR) when x>k -> Node(treeL, k, insert x treeR)
    | _                              -> tree

let rec lookup x tree =
    match tree with
    | Leaf                        -> false
    | Node(treeL, k, _) when x<k  -> lookup x treeL
    | Node(_, k, treeR) when x>k  -> lookup x treeR
    | _                           -> true

// First visit the root node, then traverse the left sub-tree in pre-order
// and finally traverse the right sub-tree in pre-order.
let rec preOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr);;

// First traverse the left sub-tree in in-order, then visit the root node and
// finally traverse the right sub-tree in in-order.
let rec inOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> (inOrder tl) @ [x] @ (inOrder tr);;

// First traverse the left sub-tree in post-order, then traverse the right
// sub-tree in post-order and finally visit the root node.
let rec postOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> (postOrder tl) @ (postOrder tr) @ [x];;
