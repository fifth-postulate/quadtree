module Main exposing (main)

import Html
import Plane exposing (boundingbox, fromPair)
import Quadtree
import Quadtree.Kernel exposing (debug)


main =
    let
        points =
            [ ( 0, 0 ), ( 37, 51 ), ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 5, 8 ) ]
                |> List.map fromPair

        box =
            boundingbox points

        quadtree =
            Maybe.map (\b -> Quadtree.for b points) box

        text
            = quadtree
              |> Maybe.map (\t -> debug (Debug.toString) t)
              |> Maybe.withDefault ""
    in
    Html.text text
