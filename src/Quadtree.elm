module Quadtree exposing (Quadtree, for, debug)

import Plane exposing (Box, Point, contains, subdivide)
import Quadtree.Kernel as Kernel


type alias Quadtree t =
    Kernel.Quadtree () t

for : Box Float -> List (Point Float) -> Quadtree (Point Float)
for box points =
    let
        containedPoints =
            points
                |> List.filter (contains box)
    in
    case containedPoints of
        [] ->
            Kernel.empty ()

        p :: [] ->
            Kernel.singleton () p

        _ as ps ->
            let
                { ne, nw, sw, se } =
                    subdivide box

                nePoints =
                    List.filter (contains ne) ps

                nwPoints =
                    List.filter (contains nw) ps

                swPoints =
                    List.filter (contains sw) ps

                sePoints =
                    List.filter (contains se) ps
            in
            Kernel.node
                ()
                (for ne nePoints)
                (for nw nwPoints)
                (for sw swPoints)
                (for se sePoints)



debug : (t -> String) -> Quadtree t -> String
debug =
    Kernel.debug (\_ -> "")

