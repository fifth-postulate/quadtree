module Plane exposing
    ( Box
    , Point
    , Subdivision
    , boundingbox
    , box
    , boxToSvg
    , contains
    , fromPair
    , pointToSvg
    , subdivide
    )

{-| The Plane module provides abstractions for working with points in the plane.
-}

import Svg
import Svg.Attributes as Attribute


{-| A `Point` represents a point in the plane.
-}
type Point t
    = Point { x : t, y : t }


{-| Create a `Point` from a tuple.
-}
fromPair : ( t, t ) -> Point t
fromPair ( x, y ) =
    Point { x = x, y = y }


{-| A representation of a rectangular region.
-}
type Box t
    = Box { xll : t, yll : t, xur : t, yur : t }


{-| Construct a box with lower left and upper right coordinates set.
-}
box : t -> t -> t -> t -> Box t
box xll yll xur yur =
    Box { xll = xll, yll = yll, xur = xur, yur = yur }


{-| Construct the [minimum bounding box](https://en.wikipedia.org/wiki/Minimum_bounding_box)

Given a list of `Point`s this will construct a `Box` that contains all the points.
If the list is empty, `boundingbox` return nothing.

-}
boundingbox : List (Point comparable) -> Maybe (Box comparable)
boundingbox points =
    case points of
        [] ->
            Nothing

        p :: ps ->
            case ps of
                [] ->
                    tinybox p
                        |> Just

                _ ->
                    boundingbox ps
                        |> Maybe.map (add p)


{-| Constructs a box containing a single point.
-}
tinybox : Point comparable -> Box comparable
tinybox (Point { x, y }) =
    box x y x y


{-| Add a `Point` to a `Box`, possible expanding it.
-}
add : Point comparable -> Box comparable -> Box comparable
add (Point { x, y }) (Box { xll, yll, xur, yur }) =
    box
        (min x xll)
        (min y yll)
        (max x xur)
        (max y yur)


{-| Determines if the `Box` contains the `Point`

Two comparison functions are passed as argument. The first determines if a point
on the left edge should be contained in the box. The second determines if a
point on the bottom edge should be contained in the box.

-}
contains : (comparable -> comparable -> Bool) -> (comparable -> comparable -> Bool) -> Box comparable -> Point comparable -> Bool
contains left bottom (Box { xll, yll, xur, yur }) (Point { x, y }) =
    left xll x
        && (x <= xur)
        && bottom yll y
        && (y <= yur)


{-| A `Subddivision` subdivdes a `Box` into quadrants
-}
type alias Subdivision t =
    { ne : Box t
    , nw : Box t
    , sw : Box t
    , se : Box t
    }


{-| Subdivides a `Box` in its four quadrants
-}
subdivide : Box Float -> Subdivision Float
subdivide (Box { xll, yll, xur, yur }) =
    let
        xmid =
            (xll + xur) / 2

        ymid =
            (yll + yur) / 2
    in
    { ne = box xmid ymid xur yur
    , nw = box xll ymid xmid yur
    , sw = box xll yll xmid ymid
    , se = box xmid yll xur ymid
    }


{-| Turn a `Point` into Svg.
-}
pointToSvg : (t -> Float) -> Point t -> List (Svg.Svg msg)
pointToSvg toFloat (Point { x, y }) =
    let
        cx =
            x
                |> toFloat
                |> String.fromFloat

        cy =
            y
                |> toFloat
                |> String.fromFloat
    in
    [ Svg.circle
        [ Attribute.cx cx
        , Attribute.cy cy
        , Attribute.r "0.3"
        ]
        []
    ]


{-| Turn a `Box` into Svg.
-}
boxToSvg : (t -> Float) -> Box t -> List (Svg.Svg msg)
boxToSvg toFloat (Box { xll, yll, xur, yur }) =
    let
        x =
            xll
                |> toFloat
                |> String.fromFloat

        y =
            yll
                |> toFloat
                |> String.fromFloat

        width =
            (toFloat xur - toFloat xll)
                |> String.fromFloat

        height =
            (toFloat yur - toFloat yll)
                |> String.fromFloat
    in
    [ Svg.rect
        [ Attribute.x x
        , Attribute.y y
        , Attribute.width width
        , Attribute.height height
        , Attribute.fill "none"
        , Attribute.stroke "black"
        , Attribute.strokeWidth "0.1"
        ]
        []
    ]
