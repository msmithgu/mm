module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



-- import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { totalTurns : Int
    , currentTurn : Int
    , secret : List Color
    , selectedIndex : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 5 1 (List.repeat 5 White) -1
    , Cmd.none
    )


allColors : List Color
allColors =
    [ White
    , Black
    , Red
    , Green
    , Blue
    , Yellow
    ]


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | White
    | Black


colorText : Color -> String
colorText c =
    case c of
        Red ->
            "red"

        Green ->
            "green"

        Blue ->
            "blue"

        Yellow ->
            "yellow"

        White ->
            "white"

        Black ->
            "black"



-- UPDATE


type Msg
    = NextTurn
    | NewGame
    | SetSecret (List Color)
    | SelectIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextTurn ->
            ( { model | currentTurn = model.currentTurn + 1 }
            , Cmd.none
            )

        NewGame ->
            ( model
            , Random.generate SetSecret (colorListGenerator 5)
            )

        SetSecret cl ->
            ( { model | secret = cl }
            , Cmd.none
            )

        SelectIndex i ->
            ( { model | selectedIndex = i }
            , Cmd.none
            )


colorListGenerator : Int -> Random.Generator (List Color)
colorListGenerator n =
    Random.list n colorGenerator


colorGenerator : Random.Generator Color
colorGenerator =
    Random.uniform White
        [ Black
        , Red
        , Green
        , Blue
        , Yellow
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "border" "6px dashed purple"
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "margin" "100px auto"
        , style "padding" "40px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "font-family" "sans-serif"
        ]
        [ h1 [] [ text "MM" ]
        , div
            [ style "border" "1px dashed #0f0"
            , style "margin" "1em"
            , style "padding" "0.5em 1em"
            , style "background" "black"
            , style "color" "#0f0"
            , style "font-family" "monospace"
            ]
            [ div [] [ text "# debug" ]
            , div [] [ text ("currentTurn: " ++ String.fromInt model.currentTurn) ]
            , div [] [ text ("selectedIndex: " ++ String.fromInt model.selectedIndex) ]
            ]
        , div [] (List.indexedMap (showIndexedColor model.selectedIndex) model.secret)
        , button [ onClick NextTurn ] [ text "Next Turn" ]
        , button [ onClick NewGame ] [ text "New Game" ]
        ]


showIndexedColor : Int -> Int -> Color -> Html Msg
showIndexedColor si i c =
    div
        [ style "display" "inline-block"
        , style "padding" "0.5em"
        , style "margin-bottom" "1em"
        , style "text-align" "center"
        , onClick (SelectIndex i)
        ]
        [ showColor c
        , text (String.fromInt i)
        , div
            []
            [ text
                (if si == i then
                    "*"

                 else
                    "-"
                )
            ]
        ]


showColor : Color -> Html msg
showColor c =
    div
        [ style "display" "block"
        , style "width" "1em"
        , style "height" "1em"
        , style "border" "1px solid black"
        , style "border-radius" "1em"
        , style "background" (colorText c)
        , style "margin" "0.5em"
        ]
        []
