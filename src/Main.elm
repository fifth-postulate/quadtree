module Main exposing (main)

import Html
import Plane exposing (boundingbox, fromPair)
import Quadtree


main =
    let
        points =
            [ ( 0, 0 ), ( 37, 51 ), ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 5, 8 ) ]
                |> List.map fromPair

        box =
            boundingbox points
    in
    Html.text "Hello, World!"
