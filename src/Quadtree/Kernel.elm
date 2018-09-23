module Quadtree.Kernel exposing (CountInfo, Quadtree, count, debug, depth, empty, node, singleton, walk)

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
singleton : i -> a -> Quadtree i a
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


{-| Keeps track of various vertices of a `Quadtree`.
-}
type alias CountInfo =
    { empty : Int
    , leaf : Int
    , node : Int
    }


{-| Count the various types of vertices
-}
count : Quadtree i a -> CountInfo
count tree =
    let
        emptyInfo =
            { empty = 0
            , leaf = 0
            , node = 0
            }

        emptyCase _ =
            { emptyInfo | empty = 1 }

        leafCase _ _ =
            { emptyInfo | leaf = 1 }

        combine left right =
            { empty = left.empty + right.empty
            , leaf = left.leaf + right.leaf
            , node = left.node + right.node
            }

        nodeCase _ ne nw sw se =
            { emptyInfo | node = 1 }
                |> combine ne
                |> combine nw
                |> combine sw
                |> combine se
    in
    walk emptyCase leafCase nodeCase tree


{-| Determine the depth of a `Quadtree`.
-}
depth : Quadtree i a -> Int
depth tree =
    let
        emptyCase _ =
            0

        leafCase _ _ =
            0

        imax a b c d =
            a
                |> max b
                |> max c
                |> max d

        nodeCase _ ne nw sw se =
            1 + imax ne nw sw se
    in
    walk emptyCase leafCase nodeCase tree


{-| Walks over a tree, combining information about Empty, Leaf and Nodes.
-}
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
