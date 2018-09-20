module Main exposing (main)

import Browser
import Html
import Html.Attributes as Attribute
import Plane exposing (Box, Point, boundingbox, boxToSvg, fromPair, pointToSvg)
import Quadtree exposing (Quadtree, quadtreeToSvg)
import Svg
import Svg.Attributes as SvgAttribute


main : Program () Model Message
main =
    let
        box =
            Plane.box 0 0 100 100
    in
    Browser.element
        { init = \_ -> ( emptyModel 10 box, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { numberOfPoints : Int
    , box : Plane.Box Float
    , quadtree : Quadtree (Point Float)
    }


emptyModel : Int -> Box Float -> Model
emptyModel numberOfPoints box =
    { numberOfPoints = numberOfPoints
    , box = box
    , quadtree = Quadtree.for box []
    }



-- Update


type Message
    = GeneratePoints


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )



-- View


view : Model -> Html.Html Message
view model =
    let
        svg =
            model.quadtree
                |> quadtreeToSvg (pointToSvg identity)
    in
    Html.div []
        [ Html.div [ Attribute.class "control" ]
            [ Html.button [] [ Html.text "generate" ]
            ]
        , Svg.svg
            [ SvgAttribute.width "640"
            , SvgAttribute.height "640"
            , SvgAttribute.viewBox "0 0 100 100"
            ]
            svg
        ]



-- let
--     points =
--         [ ( 0, 0 ), ( 37, 51 ), ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 5, 8 ) ]
--             |> List.map fromPair
--     box =
--         Just (Plane.box 0 0 51 51)
--     quadtree =
--         Maybe.map (\b -> Quadtree.for b points) box
--     svg =
--         quadtree
--             |> Maybe.map (\t -> quadtreeToSvg (pointToSvg identity) t)
--             |> Maybe.withDefault []
-- in
-- Svg.svg
--     [ Attribute.width "640"
--     , Attribute.height "640"
--     , Attribute.viewBox "0 0 51 51"
--     ]
--     svg
