module Main exposing (main)

import Browser
import Html exposing (Html, a, button, code, div, h1, hr, img, p, span, text)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (..)


main : Program () Int Msg
main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


type Msg
    = Decrement
    | Increment


update : Msg -> number -> number
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1


view : Int -> Html Msg
view model =
    div
        [ class "flex justify-center" ]
        [ div [ class "bg-gray-300 p-10 space-y-4 rounded-lg" ]
            [ img
                [ src "/logo.png"
                , class "object-contain h-48 w-96"
                ]
                []
            , h1
                [ class "text-3xl font-bold"
                ]
                [ text "Elm + Vite + Tailwind CSS!" ]
            , div []
                [ a
                    [ href "https://guide.elm-lang.org/"
                    , class "underline text-sky-600 decoration-2 hover:decoration-4"
                    ]
                    [ text "Elm Documentation" ]
                ]
            , div []
                [ a
                    [ href "https://vitejs.dev/guide/features.html"
                    , class "underline text-sky-600 decoration-2 hover:decoration-4"
                    ]
                    [ text "Vite Documentation" ]
                ]
            , div []
                [ a
                    [ href "https://tailwindcss.com/docs/installation"
                    , class "underline text-sky-600 decoration-2 hover:decoration-4"
                    ]
                    [ text "Tailwind CSS Documentation" ]
                ]
            , hr [] []
            , p [ class "flex justify-center flex-row space-x-2" ]
                [ button [ onClick Decrement, class "px-2 rounded-md bg-sky-600 hover:bg-sky-700 text-white" ] [ text "-" ]
                , div [ class "flex-none w-8 text-center" ] [ text (String.fromInt model) ]
                , button [ onClick Increment, class "px-2 rounded-md bg-sky-600 hover:bg-sky-700 text-white" ] [ text "+" ]
                ]
            , hr [] []
            , p
                []
                [ text "Edit "
                , code [] [ text "src/Main.elm" ]
                , text " to test auto refresh"
                ]
            ]
        ]
