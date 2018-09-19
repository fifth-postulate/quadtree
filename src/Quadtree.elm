module Quadtree exposing (Quadtree)

{-| A quadtree is a tree data structure in which each internal node has exactly four children
-}


type Quadtree a
    = Leaf a
    | Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a)
