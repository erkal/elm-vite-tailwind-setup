module Geometry exposing (..)


type alias Point =
    { x : Float, y : Float }


type alias Vector =
    ( Float, Float )


type alias BoundingBox =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


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


distance : Point -> Point -> Float
distance p q =
    length
        ( p.x - q.x
        , p.y - q.y
        )


length : Vector -> Float
length v =
    sqrt (dotProduct v v)


dotProduct : Vector -> Vector -> Float
dotProduct ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


boundingBoxFrom : Point -> Point -> BoundingBox
boundingBoxFrom firstPoint secondPoint =
    let
        ( x1, y1 ) =
            ( firstPoint.x, firstPoint.y )

        ( x2, y2 ) =
            ( secondPoint.x, secondPoint.y )
    in
    { minX = min x1 x2
    , maxX = max x1 x2
    , minY = min y1 y2
    , maxY = max y1 y2
    }


intersects : BoundingBox -> BoundingBox -> Bool
intersects other boundingBox =
    (boundingBox.minX <= other.maxX)
        && (boundingBox.maxX >= other.minX)
        && (boundingBox.minY <= other.maxY)
        && (boundingBox.maxY >= other.minY)
