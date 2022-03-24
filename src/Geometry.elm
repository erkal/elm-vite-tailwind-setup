module Geometry exposing (..)


type alias Point =
    { x : Float, y : Float }


type alias Vector =
    ( Float, Float )


scaleAbout : Point -> Float -> Point -> Point
scaleAbout centerPoint scale point =
    vectorFrom centerPoint point
        |> scaleBy scale
        |> (\vec -> translateBy vec centerPoint)


translateBy : Vector -> Point -> Point
translateBy ( dx, dy ) { x, y } =
    { x = x + dx
    , y = y + dy
    }


scaleBy : Float -> Vector -> Vector
scaleBy k ( x, y ) =
    ( k * x, k * y )


vectorFrom : Point -> Point -> Vector
vectorFrom p q =
    ( q.x - p.x
    , q.y - p.y
    )
