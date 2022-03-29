module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (Visibility)
import Dict
import FlowGraph exposing (FlowGraph, Node, NodeId)
import Geometry exposing (Point)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as JD
import Set exposing (Set)
import Svg exposing (Svg, circle, g, path, rect, svg)
import Svg.Attributes as SA exposing (cx, cy, d, fill, height, r, stroke, strokeDasharray, strokeWidth, width, x, y)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotViewport Dom.Viewport
    | WindowResized Int Int
    | VisibilityChanged Visibility
    | WheelDeltaY Int
    | MouseMove MousePosition
    | MouseDownOnBackgroundRectangle
    | MouseDownOnNode NodeId
    | MouseDownOnCanvas
    | KeyChanged Bool String
    | MouseUp


type alias Model =
    { flowGraph : FlowGraph () ()
    , state : State
    , screenSize : { width : Int, height : Int }
    , pressedKeys : Set String
    , selectedNodes : Set NodeId
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
    | DraggingNodes
        { mousePositionAtDragStart : Point
        , nodePositionsAtStart : List ( NodeId, Point )
        }
    | BrushingSelectionRectangle
        { mousePositionAtBrushStart : Point
        }
    | DrawingEdge
        { sourceId : NodeId
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { flowGraph = FlowGraph.exampleGraph
      , state = Idle
      , screenSize = { width = 800, height = 600 }
      , pressedKeys = Set.empty
      , selectedNodes = Set.empty
      , pan = { x = 0, y = 0 }
      , zoom = 1
      , mousePosition = { x = 0, y = 0 }
      , svgMousePosition = { x = 0, y = 0 }
      }
    , Task.perform GotViewport Dom.getViewport
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
        , Browser.Events.onResize WindowResized
        , Browser.Events.onVisibilityChange VisibilityChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport { viewport } ->
            ( { model
                | screenSize = { width = round viewport.width, height = round viewport.height }
              }
            , Cmd.none
            )

        VisibilityChanged visibility ->
            ( { model | pressedKeys = Set.empty }
            , Cmd.none
            )

        WindowResized width height ->
            ( { model
                | screenSize = { width = width, height = height }
              }
            , Cmd.none
            )

        WheelDeltaY deltaY ->
            let
                newZoom =
                    (model.zoom + 0.0005 * toFloat -deltaY)
                        |> clamp 0.25 1
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
                , selectedNodes = Set.empty
              }
            , Cmd.none
            )

        MouseDownOnCanvas ->
            ( { model
                | state =
                    if Set.member "e" model.pressedKeys then
                        case mouseOveredOutEdgeJoint model of
                            Just nodeId ->
                                DrawingEdge { sourceId = nodeId }

                            _ ->
                                model.state

                    else if Set.member "s" model.pressedKeys then
                        BrushingSelectionRectangle { mousePositionAtBrushStart = model.svgMousePosition }

                    else
                        model.state
              }
            , Cmd.none
            )

        MouseDownOnNode nodeId ->
            ( if Set.member "Shift" model.pressedKeys then
                { model
                    | selectedNodes =
                        if Set.member nodeId model.selectedNodes then
                            Set.remove nodeId model.selectedNodes

                        else
                            Set.insert nodeId model.selectedNodes
                }

              else
                { model
                    | state =
                        DraggingNodes
                            { mousePositionAtDragStart = model.svgMousePosition
                            , nodePositionsAtStart =
                                (if Set.member nodeId model.selectedNodes then
                                    Set.toList model.selectedNodes

                                 else
                                    [ nodeId ]
                                )
                                    |> List.map
                                        (\nodeId_ ->
                                            model.flowGraph
                                                |> Dict.get nodeId_
                                                |> Maybe.map .position
                                                |> Maybe.withDefault { x = 0, y = 0 }
                                                |> (\position -> ( nodeId_, position ))
                                        )
                            }
                    , selectedNodes =
                        if Set.member nodeId model.selectedNodes then
                            model.selectedNodes

                        else
                            Set.empty
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
                , selectedNodes =
                    case model.state of
                        BrushingSelectionRectangle { mousePositionAtBrushStart } ->
                            model.flowGraph
                                |> Dict.filter
                                    (\_ node ->
                                        Geometry.intersects
                                            (Geometry.boundingBoxFrom mousePositionAtBrushStart model.svgMousePosition)
                                            (FlowGraph.boundingBox node)
                                    )
                                |> Dict.keys
                                |> Set.fromList

                        _ ->
                            model.selectedNodes
                , flowGraph =
                    case model.state of
                        DraggingNodes { mousePositionAtDragStart, nodePositionsAtStart } ->
                            model.flowGraph
                                |> FlowGraph.moveNodes
                                    (nodePositionsAtStart
                                        |> List.map
                                            (Tuple.mapSecond
                                                (Geometry.translateBy
                                                    (Geometry.vectorFrom mousePositionAtDragStart model.svgMousePosition)
                                                )
                                            )
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
        [ div [ style "position" "absolute", style "margin" "10px" ]
            [ p [] [ text "Shift + click to select/deselect nodes" ]
            , p [] [ text "To draw edges, hold the `e` key down and drag with mouse" ]
            , p [] [ text "To brush a selection, hold the `s` key down and drag with the mouse" ]
            ]
        , div [ style "position" "absolute", style "margin" "10px", style "bottom" "0px" ]
            [ p [] [ text ("`state`: " ++ Debug.toString model.state) ]
            ]
        , viewCanvas model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div
        [ style "width" (String.fromInt model.screenSize.width ++ "px")
        , style "height" (String.fromInt model.screenSize.height ++ "px")
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
        (model.flowGraph |> Dict.map (viewNodeHtml model) |> Dict.values)


viewNodeHtml : Model -> NodeId -> Node () () -> Html Msg
viewNodeHtml model nodeId node =
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
        , style "background-color" "rgba(100,10,200,0.7)"
        , style "padding" "10px"
        , Html.Events.onMouseDown (MouseDownOnNode nodeId)
        , style "border" <|
            if Set.member nodeId model.selectedNodes then
                "black solid 5px"

            else
                "none"
        ]
        []


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


panAndZoomToSvgViewBox : Model -> String
panAndZoomToSvgViewBox model =
    [ model.pan.x
    , model.pan.y
    , toFloat model.screenSize.width / model.zoom
    , toFloat model.screenSize.height / model.zoom
    ]
        |> List.map String.fromFloat
        |> List.intersperse " "
        |> String.concat


svgCanvas : Model -> Html Msg
svgCanvas model =
    svg
        [ SA.width (String.fromInt model.screenSize.width)
        , SA.height (String.fromInt model.screenSize.height)
        , SA.viewBox (panAndZoomToSvgViewBox model)
        ]
        [ viewBackgroundSvgRectangleForMouseInteraction model
        , viewSelectionRectangle model

        --, circle [ cx "200", cy "300", r "10", fill "steelblue" ] []
        , g [] (model.flowGraph |> Dict.map (viewNodeSvg model) |> Dict.values)
        , viewDraggedEdge model
        ]


viewSelectionRectangle : Model -> Svg Msg
viewSelectionRectangle model =
    case model.state of
        BrushingSelectionRectangle { mousePositionAtBrushStart } ->
            let
                { maxX, minX, maxY, minY } =
                    Geometry.boundingBoxFrom mousePositionAtBrushStart model.svgMousePosition
            in
            rect
                [ x (String.fromFloat minX)
                , y (String.fromFloat minY)
                , width (String.fromFloat (maxX - minX))
                , height (String.fromFloat (maxY - minY))
                , fill "none"
                , stroke "black"
                , strokeWidth "2"
                , strokeDasharray "5,5"
                ]
                []

        _ ->
            g [] []


viewBackgroundSvgRectangleForMouseInteraction : Model -> Svg Msg
viewBackgroundSvgRectangleForMouseInteraction model =
    rect
        [ width (String.fromFloat (toFloat model.screenSize.width / model.zoom))
        , height (String.fromFloat (toFloat model.screenSize.height / model.zoom))
        , x (String.fromFloat model.pan.x)
        , y (String.fromFloat model.pan.y)
        , fill "steelblue"
        , Html.Events.onMouseDown MouseDownOnBackgroundRectangle
        ]
        []


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
