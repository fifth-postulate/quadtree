module Quadtree.Kernel exposing (Quadtree, empty, singleton, node, debug)

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
node ne nw sw se =
    Node ne nw sw se


{-| Return a debug String of the `Quadtree`
-}
debug : (t -> String) -> Quadtree t -> String
debug vizualize tree =
    case tree of
        Empty ->
            "(empty)"

        Leaf value ->
            "(leaf " ++ (vizualize value) ++ ")"

        Node ne nw sw se ->
            "(node "
                ++ debug vizualize ne
                ++ debug vizualize nw
                ++ debug vizualize sw
                ++ debug vizualize se
                ++ ")"
