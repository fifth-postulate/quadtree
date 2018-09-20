module Quadtree.Kernel exposing (Quadtree, debug, empty, node, singleton)

{-| A quadtree is a tree data structure in which each internal node has exactly four children
-}


type Quadtree i a
    = Empty i
    | Leaf i a
    | Node i (Quadtree i a) (Quadtree i a) (Quadtree i a) (Quadtree i a)


{-| Returns the empty `Quadtree`
-}
empty : i -> Quadtree i a
empty info =
    Empty info


{-| Returns a `Quadtree` with the single element
-}
singleton : i ->  a -> Quadtree i a
singleton info value =
    Leaf info value


{-| Return a `Quadtree` with the four sub-trees
-}
node : i -> Quadtree i a -> Quadtree i a -> Quadtree i a -> Quadtree i a -> Quadtree i a
node info ne nw sw se =
    Node info ne nw sw se


{-| Return a debug String of the `Quadtree`
-}
debug : (i -> String) -> (t -> String) -> Quadtree i t -> String
debug infoToString valueToString tree =
    walk
        (\i -> "(empty " ++ infoToString i ++ ")")
        (\i t -> "(leaf " ++ infoToString i ++ valueToString t ++ ")")
        (\i ne nw sw se -> "(node " ++ infoToString i ++ ne ++ nw ++ sw ++ se ++ ")")
        tree


walk : (i -> o) -> (i -> t -> o) -> (i -> o -> o -> o -> o -> o) -> Quadtree i t -> o
walk emptyCase leafCase nodeCase tree =
    case tree of
        Empty info ->
            emptyCase info

        Leaf info value ->
            leafCase info value

        Node info ne nw sw se ->
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
            nodeCase info one onw osw ose
