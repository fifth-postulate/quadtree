module Quadtree exposing (Quadtree, debug, for, quadtreeToSvg)

import Plane exposing (Box, Point, boxToSvg, contains, subdivide)
import Quadtree.Kernel as Kernel
import Svg
import Svg.Attributes as Attribute


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


{-| Return a Svg representation of the `QuadTree`.
-}
quadtreeToSvg : (t -> List (Svg.Svg msg)) -> Quadtree t -> List (Svg.Svg msg)
quadtreeToSvg valueToSvg tree =
    let
        leafToSvg box value =
            (boxToSvg identity box) ++ valueToSvg value

        nodeToSvg box ne nw sw se =
            (boxToSvg identity box) ++ ne ++ nw ++ sw ++ se
    in
    Kernel.walk (boxToSvg identity) leafToSvg nodeToSvg tree


{-| Debug a `Quadtree`.
-}
debug : (t -> String) -> Quadtree t -> String
debug =
    Kernel.debug (\_ -> "")
