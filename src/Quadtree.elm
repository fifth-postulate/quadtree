module Quadtree exposing (Quadtree, debug, for)

import Plane exposing (Box, Point, contains, subdivide)
import Quadtree.Kernel as Kernel


{-| Alias for the `Kernel.Quadtree`.

This stores information about the containing `Box`.

-}
type alias Quadtree t =
    Kernel.Quadtree (Box Float) t


{-| Create a `Quadtree` from a `Box` and the points contained in that `Box`.
-}
for : Box Float -> List (Point Float) -> Quadtree (Point Float)
for box points =
    let
        containedPoints =
            points
                |> List.filter (contains (<=) (<=) box)
    in
    case containedPoints of
        [] ->
            Kernel.empty box

        p :: [] ->
            Kernel.singleton box p

        _ as ps ->
            let
                { ne, nw, sw, se } =
                    subdivide box

                nePoints =
                    List.filter (contains (<) (<) ne) ps

                nwPoints =
                    List.filter (contains (<=) (<) nw) ps

                swPoints =
                    List.filter (contains (<=) (<=) sw) ps

                sePoints =
                    List.filter (contains (<) (<=) se) ps
            in
            Kernel.node
                box
                (for ne nePoints)
                (for nw nwPoints)
                (for sw swPoints)
                (for se sePoints)


{-| Debug a `Quadtree`
-}
debug : (t -> String) -> Quadtree t -> String
debug =
    Kernel.debug (\_ -> "")
