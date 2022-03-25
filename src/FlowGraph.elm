module FlowGraph exposing (..)

import Dict exposing (Dict)
import Geometry exposing (Point)


type alias FlowGraph n e =
    Dict NodeId (Node n e)


type alias NodeId =
    Int


type alias Node n e =
    { nodeData : n
    , outEdges : List (Edge e)

    --
    , position : Point
    , width : Float
    , height : Float
    }


type alias Edge e =
    { edgeData : e
    , target : NodeId

    --
    , yCoordinateOnSource : Float
    , yCoordinateOnTarget : Float
    }


empty : FlowGraph n e
empty =
    Dict.empty


exampleGraph : FlowGraph () ()
exampleGraph =
    let
        makeNode pos nodeId edgeTargets =
            ( nodeId
            , { nodeData = ()
              , outEdges =
                    edgeTargets
                        |> List.map
                            (\target ->
                                { edgeData = ()
                                , target = target

                                --
                                , yCoordinateOnSource = 30
                                , yCoordinateOnTarget = 30
                                }
                            )

              --
              , position = pos
              , width = 100
              , height = 100
              }
            )
    in
    Dict.fromList
        [ makeNode { x = 100, y = 300 } 0 [ 1, 2 ]
        , makeNode { x = 500, y = 600 } 1 [ 2 ]
        , makeNode { x = 800, y = 500 } 2 []
        ]
