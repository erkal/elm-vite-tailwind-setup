module FlowGraph exposing (..)

import Dict exposing (Dict)
import Geometry exposing (BoundingBox, Point)
import Set exposing (Set)


type alias FlowGraph =
    Dict NodeId Node


type alias NodeId =
    Int


type alias Node =
    { position : Point
    , width : Float
    , height : Float
    , outEdges : List Edge
    }


type alias Edge =
    { edgeType : EdgeType
    , target : NodeId
    , offsetTop : Float
    , offsetLeft : Float
    }


type EdgeType
    = Start
    | Basic
    | Intent_Match
    | Intent_NoMatch
    | Wait_UserInput
    | Wait_Timeout
    | SplitBy_Condition {- TODO: Type for Condition -}
    | SplitBy_Percentage Float
    | Choice String
    | Action_Success
    | Action_Failure


exampleGraph : FlowGraph
exampleGraph =
    let
        makeNode pos nodeId edgeTargets =
            ( nodeId
            , { position = pos
              , width = 240
              , height = 160
              , outEdges =
                    edgeTargets
                        |> List.map
                            (\target ->
                                { edgeType = Basic
                                , target = target
                                , offsetTop = 0
                                , offsetLeft = 0
                                }
                            )
              }
            )
    in
    Dict.fromList
        [ makeNode { x = 100, y = 100 } 0 [ 1 ]
        , makeNode { x = 500, y = 400 } 1 [ 2 ]
        , makeNode { x = 800, y = 300 } 2 [ 3 ]
        , makeNode { x = 1000, y = 100 } 3 [ 1 ]
        ]


moveNodes : List ( NodeId, Point ) -> FlowGraph -> FlowGraph
moveNodes l flowGraph =
    l
        |> List.foldl
            (\( nodeId, position ) acc -> Dict.update nodeId (Maybe.map (\node -> { node | position = position })) acc)
            flowGraph


insertEdge : NodeId -> NodeId -> FlowGraph -> FlowGraph
insertEdge sourceId targetId flowGraph =
    flowGraph
        |> Dict.update sourceId
            (Maybe.map
                (\node ->
                    { node
                        | outEdges =
                            { edgeType = Basic
                            , target = targetId
                            , offsetTop = 0
                            , offsetLeft = 0
                            }
                                :: node.outEdges
                    }
                )
            )


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
        |> Dict.map (\_ node -> { node | outEdges = node.outEdges |> List.filter (\{ target } -> Set.member target nodeIds) })


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
                                    |> List.map (\edge -> { edge | target = Dict.get edge.target newNodeIds |> Maybe.withDefault 0 })
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
        , offsetLeft : Float
        }



-- Set and get data for edge joints in order to position the svg edges correctly


applyOutEdgeCoordinatesFromPort : OutEdgeDataFromPort -> FlowGraph -> FlowGraph
applyOutEdgeCoordinatesFromPort outEdgeDataFromPort flowGraph =
    outEdgeDataFromPort
        |> List.foldl
            (\{ id, offsetTop, offsetLeft } acc ->
                let
                    nodeId =
                        id |> String.toInt |> Maybe.withDefault 0

                    updateEdge edge =
                        { edge | offsetTop = offsetTop, offsetLeft = offsetLeft }

                    updateNode node =
                        { node | outEdges = node.outEdges |> List.map updateEdge }
                in
                acc |> Dict.update nodeId (Maybe.map updateNode)
            )
            flowGraph
