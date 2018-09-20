module Quadtree.Kernel exposing (Quadtree, debug, empty, node, singleton)

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
    walk
        (\_ -> "(empty)")
        (\t -> "(leaf " ++ vizualize t ++ ")")
        (\ne nw sw se -> "(node " ++ ne ++ nw ++ sw ++ se ++ ")")
        tree


walk : (() -> o) -> (t -> o) -> (o -> o -> o -> o -> o) -> Quadtree t -> o
walk emptyCase leafCase nodeCase tree =
    case tree of
        Empty ->
            emptyCase ()

        Leaf value ->
            leafCase value

        Node ne nw sw se ->
            let
                one =
                    walk emptyCase leafCase nodeCase ne

                onw =
                    walk emptyCase leafCase nodeCase nw

                osw =
                    walk emptyCase leafCase nodeCase sw

                ose =
                    walk emptyCase leafCase nodeCase se
            in
            nodeCase one onw osw ose
