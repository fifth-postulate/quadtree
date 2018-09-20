module Main exposing (main)

import Html
import Plane exposing (boundingbox, boxToSvg, fromPair, pointToSvg)
import Quadtree exposing (quadtreeToSvg)
import Svg
import Svg.Attributes as Attribute


main =
    let
        points =
            [ ( 0, 0 ), ( 37, 51 ), ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 5, 8 ) ]
                |> List.map fromPair

        box =
            Just (Plane.box 0 0 51 51)

        quadtree =
            Maybe.map (\b -> Quadtree.for b points) box

        svg =
            quadtree
                |> Maybe.map (\t -> quadtreeToSvg (pointToSvg identity) t)
                |> Maybe.withDefault []
    in
    Svg.svg
        [ Attribute.width "640"
        , Attribute.height "640"
        , Attribute.viewBox "0 0 51 51"
        ]
        svg
