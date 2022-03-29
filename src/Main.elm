module Main exposing (main)

import Browser
import Browser.Events
import Dict
import FlowGraph exposing (FlowGraph, Node, NodeId)
import Geometry exposing (Point)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg, circle, g, path, rect, svg)
import Svg.Attributes as SA exposing (cx, cy, d, fill, height, r, stroke, strokeWidth, width)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = WheelDeltaY Int
    | MouseMove MousePosition
    | MouseDownOnBackgroundRectangle
    | MouseDownOnNode NodeId Point
    | MouseDownOnCanvas
    | KeyChanged Bool String
    | MouseUp


type alias Model =
    { flowGraph : FlowGraph () ()
    , state : State
    , pressedKeys : Set String
    , pan :
        -- svg coordinates of the top left corner of the browser window
        Point
    , zoom : Float
    , mousePosition : MousePosition
    , svgMousePosition : Point
    }


type State
    = Idle
    | Panning
        { mousePositionAtPanStart : MousePosition
        , panAtStart : Point
        }
    | DraggingNode
        { nodeId : NodeId
        , brushStart : Point
        , nodePositionAtStart : Point
        }
    | DrawingEdge { sourceId : NodeId }


init : () -> ( Model, Cmd Msg )
init () =
    ( { flowGraph = FlowGraph.exampleGraph
      , state = Idle
      , pressedKeys = Set.empty
      , pan = { x = 0, y = 0 }
      , zoom = 1
      , mousePosition = { x = 0, y = 0 }
      , svgMousePosition = { x = 0, y = 0 }
      }
    , Cmd.none
    )


type alias MousePosition =
    { x : Int, y : Int }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseMove
            (JD.map MouseMove (JD.map2 MousePosition (JD.field "clientX" JD.int) (JD.field "clientY" JD.int)))
        , Browser.Events.onMouseUp
            (JD.succeed MouseUp)
        , Browser.Events.onKeyUp (JD.map (KeyChanged False) (JD.field "key" JD.string))
        , Browser.Events.onKeyDown (JD.map (KeyChanged True) (JD.field "key" JD.string))
        ]


minZoom =
    0.25


maxZoom =
    1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WheelDeltaY deltaY ->
            let
                newZoom =
                    (model.zoom + 0.0005 * toFloat -deltaY)
                        |> clamp minZoom maxZoom
            in
            ( { model
                | zoom = newZoom
                , pan = model.pan |> Geometry.scaleAbout model.svgMousePosition (model.zoom / newZoom)
              }
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model
                | pressedKeys =
                    if isDown then
                        Set.insert key model.pressedKeys

                    else
                        Set.remove key model.pressedKeys
              }
            , Cmd.none
            )

        MouseDownOnBackgroundRectangle ->
            ( { model
                | state =
                    Panning
                        { mousePositionAtPanStart = model.mousePosition
                        , panAtStart = model.pan
                        }
              }
            , Cmd.none
            )

        MouseDownOnCanvas ->
            ( { model
                | state =
                    if Set.member "Shift" model.pressedKeys then
                        case mouseOveredOutEdgeJoint model of
                            Just nodeId ->
                                DrawingEdge { sourceId = nodeId }

                            _ ->
                                model.state

                    else
                        model.state
              }
            , Cmd.none
            )

        MouseDownOnNode nodeId nodePositionAtStart ->
            ( { model
                | state =
                    DraggingNode
                        { nodeId = nodeId
                        , brushStart = model.svgMousePosition
                        , nodePositionAtStart = nodePositionAtStart
                        }
              }
            , Cmd.none
            )

        MouseMove newMousePosition ->
            ( { model
                | mousePosition = newMousePosition
                , svgMousePosition =
                    { x = toFloat newMousePosition.x
                    , y = toFloat newMousePosition.y
                    }
                        |> Geometry.scaleAbout { x = 0, y = 0 } (1 / model.zoom)
                        |> Geometry.translateBy ( model.pan.x, model.pan.y )
                , flowGraph =
                    case model.state of
                        DraggingNode { nodeId, brushStart, nodePositionAtStart } ->
                            model.flowGraph
                                |> FlowGraph.moveNode nodeId
                                    (nodePositionAtStart
                                        |> Geometry.translateBy
                                            (Geometry.vectorFrom brushStart model.svgMousePosition)
                                    )

                        _ ->
                            model.flowGraph
                , pan =
                    case model.state of
                        Panning { mousePositionAtPanStart, panAtStart } ->
                            let
                                toPoint : MousePosition -> Point
                                toPoint pos =
                                    { x = toFloat pos.x
                                    , y = toFloat pos.y
                                    }

                                delta =
                                    Geometry.vectorFrom (toPoint newMousePosition) (toPoint mousePositionAtPanStart)
                                        |> Geometry.scaleBy (1 / model.zoom)
                            in
                            panAtStart |> Geometry.translateBy delta

                        _ ->
                            model.pan
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model
                | state = Idle
                , flowGraph =
                    case model.state of
                        DrawingEdge { sourceId } ->
                            case mouseOveredInEdgeJoint model of
                                Just targetId ->
                                    model.flowGraph |> FlowGraph.insertEdge sourceId targetId ()

                                _ ->
                                    model.flowGraph

                        _ ->
                            model.flowGraph
              }
            , Cmd.none
            )


mouseOveredInEdgeJoint : Model -> Maybe NodeId
mouseOveredInEdgeJoint model =
    model.flowGraph
        |> Dict.filter
            (\_ node ->
                Geometry.distance (FlowGraph.inEdgeJointCoordinates node) model.svgMousePosition < 15
            )
        |> Dict.keys
        |> List.head


mouseOveredOutEdgeJoint : Model -> Maybe NodeId
mouseOveredOutEdgeJoint model =
    model.flowGraph
        |> Dict.filter
            (\_ node ->
                Geometry.distance (FlowGraph.outEdgeJointCoordinates node) model.svgMousePosition < 15
            )
        |> Dict.keys
        |> List.head


view : Model -> Html Msg
view model =
    div []
        [ div [ style "position" "absolute" ]
            [ p [] [ text (Debug.toString model.state) ]
            , p [] [ text (Debug.toString model.pressedKeys) ]
            ]
        , viewCanvas model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div
        [ style "width" (String.fromInt canvasWidthInPixels ++ "px")
        , style "height" (String.fromInt canvasHeightInPixels ++ "px")
        , style "background-color" "lightgray"
        , style "overflow" "hidden"
        , Html.Events.onMouseDown MouseDownOnCanvas
        , Html.Events.on "wheel" (JD.map WheelDeltaY (JD.field "deltaY" JD.int))
        ]
        [ htmlCanvas model
        , svgCanvas model
        ]


htmlCanvas : Model -> Html Msg
htmlCanvas model =
    div
        [ style "position" "absolute"
        , style "transform" (panAndZoomToDivTransform model.pan model.zoom)
        ]
        (model.flowGraph |> Dict.map viewNodeHtml |> Dict.values)


viewNodeHtml : NodeId -> Node () () -> Html Msg
viewNodeHtml nodeId node =
    div
        [ style "position" "absolute"
        , style "transform"
            ("translate("
                ++ String.fromFloat node.position.x
                ++ "px,"
                ++ String.fromFloat node.position.y
                ++ "px)"
            )
        , style "width" (String.fromFloat node.width ++ "px")
        , style "height" (String.fromFloat node.height ++ "px")
        , style "background-color" "orange"
        , style "opacity" "0.4"
        , style "padding" "10px"
        , Html.Events.onMouseDown (MouseDownOnNode nodeId node.position)
        ]
        [ text "Html"
        ]


panAndZoomToDivTransform : Point -> Float -> String
panAndZoomToDivTransform pan zoom =
    let
        ( translateX, translateY ) =
            ( String.fromFloat -(pan.x * zoom)
            , String.fromFloat -(pan.y * zoom)
            )
    in
    "translate("
        ++ translateX
        ++ "px, "
        ++ translateY
        ++ "px) "
        ++ "scale("
        ++ String.fromFloat zoom
        ++ ")"


panAndZoomToSvgViewBox : Point -> Float -> String
panAndZoomToSvgViewBox pan zoom =
    [ pan.x
    , pan.y
    , toFloat canvasWidthInPixels / zoom
    , toFloat canvasHeightInPixels / zoom
    ]
        |> List.map String.fromFloat
        |> List.intersperse " "
        |> String.concat


canvasWidthInPixels =
    1200


canvasHeightInPixels =
    600


svgCanvas : Model -> Html Msg
svgCanvas model =
    svg
        [ SA.width (String.fromInt canvasWidthInPixels)
        , SA.height (String.fromInt canvasHeightInPixels)
        , SA.viewBox (panAndZoomToSvgViewBox model.pan model.zoom)
        ]
        [ rect
            [ width "1200"
            , height "800"
            , fill "gray"
            , Html.Events.onMouseDown MouseDownOnBackgroundRectangle
            ]
            []

        --, circle [ cx "200", cy "300", r "10", fill "steelblue" ] []
        , g [] (model.flowGraph |> Dict.map (viewNodeSvg model) |> Dict.values)
        , viewDraggedEdge model
        ]


viewDraggedEdge : Model -> Svg Msg
viewDraggedEdge model =
    case model.state of
        DrawingEdge { sourceId } ->
            Dict.get sourceId model.flowGraph
                |> Maybe.map
                    (\node ->
                        viewEdgeSvg (FlowGraph.outEdgeJointCoordinates node) model.svgMousePosition
                    )
                |> Maybe.withDefault (g [] [])

        _ ->
            g [] []


viewEdgeSvg : Point -> Point -> Svg Msg
viewEdgeSvg startPoint endPoint =
    let
        dxdy =
            endPoint
                |> Geometry.translateBy ( -startPoint.x, -startPoint.y )

        dx1dy1 =
            { x = 0.5 * dxdy.x
            , y = 0
            }

        dx2dy2 =
            { x = dxdy.x - 0.5 * dxdy.x
            , y = dxdy.y
            }

        toStr { x, y } =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    path
        [ d <|
            "M "
                ++ toStr startPoint
                ++ " c "
                ++ toStr dx1dy1
                ++ " "
                ++ toStr dx2dy2
                ++ " "
                ++ toStr dxdy
        , stroke "black"
        , fill "none"
        , strokeWidth "4"
        ]
        []


viewNodeSvg : Model -> NodeId -> Node () () -> Svg Msg
viewNodeSvg model nodeId node =
    let
        viewEdgeSvg_ target _ =
            let
                endPoint =
                    model.flowGraph
                        |> Dict.get target
                        |> Maybe.map (\n -> n.position |> Geometry.translateBy ( 0, 0.5 * n.height ))
                        |> Maybe.withDefault { x = 0, y = 0 }
            in
            viewEdgeSvg (FlowGraph.outEdgeJointCoordinates node) endPoint
    in
    g [] (node.outEdges |> Dict.map viewEdgeSvg_ |> Dict.values)
