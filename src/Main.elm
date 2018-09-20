module Main exposing (main)

import Browser
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Plane exposing (Box, Point, boundingbox, boxToSvg, fromPair, point, pointToSvg)
import Quadtree exposing (Quadtree, quadtreeToSvg)
import Random
import Svg
import Svg.Attributes as SvgAttribute


main : Program () Model Message
main =
    Browser.element
        { init = \_ -> ( emptyModel 10 100, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { numberOfPoints : Int
    , size : Float
    , box : Plane.Box Float
    , quadtree : Quadtree (Point Float)
    }


emptyModel : Int -> Float -> Model
emptyModel numberOfPoints size =
    let
        box =
            Plane.box 0 0 size size
    in
    { numberOfPoints = numberOfPoints
    , size = size
    , box = box
    , quadtree = Quadtree.for box []
    }



-- Update


type Message
    = GeneratePoints
    | Points (List (Point Float))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GeneratePoints ->
            let
                pointGenerator =
                    Random.float 0 model.size
                        |> point

                pointsGenerator =
                    Random.list model.numberOfPoints pointGenerator

                command =
                    Random.generate Points pointsGenerator
            in
            ( model, command )

        Points points ->
            let
                quadtree =
                    Quadtree.for model.box points

                m =
                    { model | quadtree = quadtree }
            in
            ( m, Cmd.none )



-- View


view : Model -> Html.Html Message
view model =
    let
        svg =
            model.quadtree
                |> quadtreeToSvg (pointToSvg identity)

        viewBox =
            [ 0, 0, model.size, model.size ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    Html.div []
        [ Html.div [ Attribute.class "control" ]
            [ Html.button [ Event.onClick GeneratePoints ] [ Html.text "generate" ]
            ]
        , Svg.svg
            [ SvgAttribute.width "640"
            , SvgAttribute.height "640"
            , SvgAttribute.viewBox viewBox
            ]
            svg
        ]
