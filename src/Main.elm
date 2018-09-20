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
    | Points (List (Point Float))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        GeneratePoints ->
            let
                pointGenerator =
                    Random.float 0 100
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
                    Quadtree.for model.box (Debug.log "points: " points)

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
    in
    Html.div []
        [ Html.div [ Attribute.class "control" ]
            [ Html.button [ Event.onClick GeneratePoints ] [ Html.text "generate" ]
            ]
        , Svg.svg
            [ SvgAttribute.width "640"
            , SvgAttribute.height "640"
            , SvgAttribute.viewBox "0 0 100 100"
            ]
            svg
        ]
