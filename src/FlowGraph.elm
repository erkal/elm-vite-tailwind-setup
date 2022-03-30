module FlowGraph exposing (..)

import Dict exposing (Dict)
import Geometry exposing (BoundingBox, Point)
import Set exposing (Set)


type alias FlowGraph n e =
    Dict NodeId (Node n e)


type alias NodeId =
    String


type alias Node n e =
    { nodeData : n
    , outEdges : Dict NodeId e

    --
    , position : Point
    , width : Float
    , height : Float
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
                        |> List.map (\target -> ( target, () ))
                        |> Dict.fromList

              --
              , position = pos
              , width = 200
              , height = 100
              }
            )
    in
    Dict.fromList
        [ makeNode { x = 100, y = 100 } "0" [ "1", "2" ]
        , makeNode { x = 500, y = 400 } "1" [ "2" ]
        , makeNode { x = 800, y = 300 } "2" []
        , makeNode { x = 1000, y = 100 } "3" []
        ]


outEdgeJointCoordinates : Node n e -> Point
outEdgeJointCoordinates node =
    node.position |> Geometry.translateBy ( node.width, 0.5 * node.height )


inEdgeJointCoordinates : Node n e -> Point
inEdgeJointCoordinates node =
    node.position |> Geometry.translateBy ( 0, 0.5 * node.height )


moveNodes : List ( NodeId, Point ) -> FlowGraph () () -> FlowGraph () ()
moveNodes l flowGraph =
    l
        |> List.foldl
            (\( nodeId, position ) acc -> Dict.update nodeId (Maybe.map (\node -> { node | position = position })) acc)
            flowGraph


insertEdge : NodeId -> NodeId -> e -> FlowGraph n e -> FlowGraph n e
insertEdge sourceId targetId edge flowGraph =
    flowGraph
        |> Dict.update sourceId
            (Maybe.map (\node -> { node | outEdges = node.outEdges |> Dict.insert targetId edge }))


boundingBox : Node n e -> BoundingBox
boundingBox node =
    { minX = node.position.x
    , maxX = node.position.x + node.width
    , minY = node.position.y
    , maxY = node.position.y + node.height
    }


inducedSubgraph : Set NodeId -> FlowGraph n e -> FlowGraph n e
inducedSubgraph nodeIds flowGraph =
    flowGraph
        |> Dict.filter (\nodeId _ -> Set.member nodeId nodeIds)
        |> Dict.map (\_ node -> { node | outEdges = node.outEdges |> Dict.filter (\nodeId _ -> Set.member nodeId nodeIds) })


duplicateSubgraph : Set NodeId -> FlowGraph n e -> FlowGraph n e
duplicateSubgraph nodeIds flowGraph =
    let
        newName nodeId =
            -- TODO
            nodeId
    in
    Dict.union flowGraph
        (inducedSubgraph nodeIds flowGraph
            |> Dict.toList
            |> List.map
                (Tuple.mapBoth
                    ((++) "copy-")
                    (\node ->
                        { node
                            | outEdges =
                                node.outEdges
                                    |> Dict.toList
                                    |> List.map (Tuple.mapFirst ((++) "copy-"))
                                    |> Dict.fromList
                        }
                    )
                )
            |> Dict.fromList
        )
