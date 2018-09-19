module Plane exposing (Box, Point, boundingbox, box, contains, fromPair, subdivide)

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
-}
contains : Box Float -> Point Float -> Bool
contains (Box { xll, yll, xur, yur }) (Point { x, y }) =
    (xll <= x)
        && (x <= xur)
        && (yll <= y)
        && (y <= yur)


{-| Subdivides a `Box` in its four quadrants
-}
subdivide : Box Float -> ( ( Box Float, Box Float ), ( Box Float, Box Float ) )
subdivide (Box { xll, yll, xur, yur }) =
    let
        xmid =
            (xll + xur) / 2

        ymid =
            (yll + yur) / 2
    in
    ( ( box xmid ymid xur yur
      , box xll ymid xmid yur
      )
    , ( box xll yll xmid ymid
      , box xmid yll xur ymid
      )
    )
