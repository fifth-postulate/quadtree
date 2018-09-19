module Quadtree exposing (Quadtree, for)

import Plane exposing (Box, Point, contains, subdivide)
import Quadtree.Kernel as Kernel


type alias Quadtree t =
    Kernel.Quadtree t


for : Box Float -> List (Point Float) -> Quadtree (Point Float)
for box points =
    let
        containedPoints =
            points
                |> List.filter (contains box)
    in
    case containedPoints of
        [] ->
            Kernel.empty

        p :: [] ->
            Kernel.singleton p

        _ as ps ->
            let
                ( ( nwBox, neBox ), ( seBox, swBox ) ) =
                    subdivide box

                nwPoints =
                    List.filter (contains nwBox) ps

                nePoints =
                    List.filter (contains neBox) ps

                sePoints =
                    List.filter (contains seBox) ps

                swPoints =
                    List.filter (contains swBox) ps
            in
            Kernel.node
                (for nwBox nwPoints)
                (for neBox nePoints)
                (for seBox sePoints)
                (for swBox swPoints)
