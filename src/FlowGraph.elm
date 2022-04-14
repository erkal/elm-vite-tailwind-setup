module FlowGraph exposing (..)

import Dict exposing (Dict)
import Geometry exposing (BoundingBox, Point)
import Set exposing (Set)


type alias FlowGraph =
    Dict NodeId Node


type alias NodeId =
    Int


type alias Node =
    { outEdges : Dict NodeId Float

    {- `offsetTop` value for positioning the svg edges -}
    --
    , position : Point
    , width : Float
    , height : Float
    }


empty : FlowGraph
empty =
    Dict.empty


exampleGraph : FlowGraph
exampleGraph =
    let
        makeNode pos nodeId edgeTargets =
            ( nodeId
            , { outEdges =
                    edgeTargets
                        |> List.map (\target -> ( target, 0 ))
                        |> Dict.fromList

              --
              , position = pos
              , width = 240
              , height = 160
              }
            )
    in
    Dict.fromList
        [ makeNode { x = 100, y = 100 } 0 [ 1, 2 ]
        , makeNode { x = 500, y = 400 } 1 [ 2 ]
        , makeNode { x = 800, y = 300 } 2 []
        , makeNode { x = 1000, y = 100 } 3 []
        ]


moveNodes : List ( NodeId, Point ) -> FlowGraph -> FlowGraph
moveNodes l flowGraph =
    l
        |> List.foldl
            (\( nodeId, position ) acc -> Dict.update nodeId (Maybe.map (\node -> { node | position = position })) acc)
            flowGraph


insertEdge : NodeId -> NodeId -> e -> FlowGraph -> FlowGraph
insertEdge sourceId targetId edge flowGraph =
    flowGraph
        |> Dict.update sourceId
            (Maybe.map (\node -> { node | outEdges = node.outEdges |> Dict.insert targetId e }))


boundingBox : Node -> BoundingBox
boundingBox node =
    { minX = node.position.x
    , maxX = node.position.x + node.width
    , minY = node.position.y
    , maxY = node.position.y + node.height
    }


inducedSubgraph : Set NodeId -> FlowGraph -> FlowGraph
inducedSubgraph nodeIds flowGraph =
    flowGraph
        |> Dict.filter (\nodeId _ -> Set.member nodeId nodeIds)
        |> Dict.map (\_ node -> { node | outEdges = node.outEdges |> Dict.filter (\nodeId _ -> Set.member nodeId nodeIds) })


duplicateSubgraph : Set NodeId -> FlowGraph -> ( FlowGraph, List NodeId )
duplicateSubgraph nodeIds flowGraph =
    let
        maxNodeId : NodeId
        maxNodeId =
            flowGraph
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        newNodeIds : Dict NodeId NodeId
        newNodeIds =
            nodeIds
                |> Set.toList
                |> List.indexedMap (\i nodeId -> ( nodeId, maxNodeId + 1 + i ))
                |> Dict.fromList
    in
    ( Dict.union flowGraph
        (inducedSubgraph nodeIds flowGraph
            |> Dict.toList
            |> List.map
                (Tuple.mapBoth
                    (\nodeId -> Dict.get nodeId newNodeIds |> Maybe.withDefault 0)
                    (\node ->
                        { node
                            | outEdges =
                                node.outEdges
                                    |> Dict.toList
                                    |> List.map
                                        (\( nodeId_, y ) ->
                                            ( Dict.get nodeId_ newNodeIds |> Maybe.withDefault 0
                                            , y
                                            )
                                        )
                                    |> Dict.fromList
                        }
                    )
                )
            |> Dict.fromList
        )
    , Dict.values newNodeIds
    )


type alias OutEdgeDataFromPort =
    List
        { id : String
        , offsetTop : Float
        }



-- Set and get data for edge joints in order to position the svg edges correctly


applyOutEdgeCoordinatesFromPort : OutEdgeDataFromPort -> FlowGraph -> FlowGraph
applyOutEdgeCoordinatesFromPort outEdgeDataFromPort flowGraph =
    outEdgeDataFromPort
        |> List.foldl
            (\{ id, offsetTop } acc ->
                let
                    nodeId =
                        id |> String.toInt |> Maybe.withDefault 0
                in
                acc
                    |> Dict.update nodeId
                        (Maybe.map (\node -> { node | outEdges = node.outEdges |> Dict.map (\_ _ -> offsetTop) }))
            )
            flowGraph


outEdgeJointCoordinatesForSVGDrawing : Node -> Point
outEdgeJointCoordinatesForSVGDrawing node =
    node.position
        |> Geometry.translateBy
            ( node.width
            , node.outEdges |> Dict.values |> List.head |> Maybe.withDefault 0
            )


inEdgeJointCoordinatesForSVGDrawing : Node -> Point
inEdgeJointCoordinatesForSVGDrawing node =
    node.position |> Geometry.translateBy ( 0, 16 )
