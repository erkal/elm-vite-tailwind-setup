module Main exposing (main)

import Browser
import Browser.Events
import Dict
import FlowGraph exposing (FlowGraph, Node, NodeId)
import Geometry exposing (Point)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as JD
import Svg exposing (circle, rect, svg)
import Svg.Attributes as SA exposing (cx, cy, fill, height, r, width)


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
    | MouseUp


type alias Model =
    { flowGraph : FlowGraph () ()
    , state : State
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


init : () -> ( Model, Cmd Msg )
init () =
    ( { flowGraph = FlowGraph.exampleGraph
      , state = Idle
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
            ( { model | state = Idle }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ viewCanvas model ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div
        [ style "width" (String.fromInt canvasWidthInPixels ++ "px")
        , style "height" (String.fromInt canvasHeightInPixels ++ "px")
        , style "background-color" "lightgray"
        , style "overflow" "hidden"
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
        (model.flowGraph |> Dict.map viewNode |> Dict.values)


viewNode : NodeId -> Node () () -> Html Msg
viewNode nodeId node =
    div
        [ style "position" "absolute"
        , style "transform"
            ("translate("
                ++ String.fromFloat node.position.x
                ++ "px,"
                ++ String.fromFloat node.position.y
                ++ "px)"
            )
        , style "width" "200px"
        , style "height" "100px"
        , style "background-color" "red"
        , style "opacity" "0.6"
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
        , circle [ cx "200", cy "300", r "10", fill "steelblue" ] []
        , circle [ cx "240", cy "310", r "20", fill "steelblue" ] []
        ]
