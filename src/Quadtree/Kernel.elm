module Quadtree.Kernel exposing (Quadtree, empty, singleton, node)

{-| A quadtree is a tree data structure in which each internal node has exactly four children
-}


type Quadtree a
    = Empty
    | Leaf a
    | Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a)


{-| Returns the empty `Quadtree`
-}
empty : Quadtree a
empty =
    Empty


{-| Returns a `Quadtree` with the single element
-}
singleton : a -> Quadtree a
singleton value =
    Leaf value


{-| Return a `Quadtree` with the four sub-trees
-}
node : Quadtree a -> Quadtree a -> Quadtree a -> Quadtree a -> Quadtree a
node nw ne se sw =
    Node nw ne se sw
