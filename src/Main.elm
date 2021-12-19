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
    { secret : Cypher
    , guesses : List Cypher
    , selectedIndex : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model blankCypher [ blankCypher ] -1
    , Cmd.none
    )


blankCypher : Cypher
blankCypher =
    List.repeat 4 White


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


type alias Cypher =
    List Color


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
    = NextGuess
    | NewGame
    | SetSecret (List Color)
    | SelectIndex Int
    | UpdateColor Color


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextGuess ->
            ( { model | guesses = blankCypher :: model.guesses }
            , Cmd.none
            )

        NewGame ->
            ( { model | guesses = [ blankCypher ] }
            , Random.generate SetSecret (colorListGenerator 4)
            )

        SetSecret cl ->
            ( { model | secret = cl }
            , Cmd.none
            )

        SelectIndex i ->
            ( { model | selectedIndex = i }
            , Cmd.none
            )

        UpdateColor col ->
            ( { model | selectedIndex = -1, guesses = updateGuess model.guesses 0 model.selectedIndex col }
            , Cmd.none
            )


updateGuess : List Cypher -> Int -> Int -> Color -> List Cypher
updateGuess guessList selectedGuessIndex selectedColorIndex col =
    let
        guessUpdate i guess =
            if i == selectedGuessIndex then
                updateCypher guess col selectedColorIndex

            else
                guess
    in
    List.indexedMap guessUpdate guessList


updateCypher : Cypher -> Color -> Int -> Cypher
updateCypher cyph colorToUpdate selectedIndex =
    let
        colorUpdate i col =
            if i == selectedIndex then
                colorToUpdate

            else
                col
    in
    List.indexedMap colorUpdate cyph


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
        , style "margin" "40px auto"
        , style "padding" "40px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "font-family" "sans-serif"
        ]
        [ h1 [] [ text "MM" ]
        , div [] (List.map showColor model.secret)
        , div
            [ style "border" "1px dashed #0f0"
            , style "margin" "1em"
            , style "padding" "0.5em 1em"
            , style "background" "black"
            , style "color" "#0f0"
            , style "font-family" "monospace"
            ]
            [ div [] [ text "# debug" ]
            , div [] [ text ("selectedIndex: " ++ String.fromInt model.selectedIndex) ]
            ]
        , div [ style "margin" "1em" ]
            [ button
                [ onClick NewGame ]
                [ text "New Game" ]
            ]
        , showColorPicker
        , showGuesses model
        ]


showGuesses : Model -> Html Msg
showGuesses model =
    div [] (List.indexedMap (showIndexedGuess model) model.guesses)


showIndexedGuess : Model -> Int -> Cypher -> Html Msg
showIndexedGuess model guessIndex guess =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ div
            [ style "margin" "0.5em" ]
            [ text (String.fromInt (List.length model.guesses - guessIndex)) ]
        , div
            [ style "border-left" "4px solid transparent"
            , style "border-right" "4px solid transparent"
            , style "border-color"
                (if guessIndex == 0 then
                    "black"

                 else
                    "transparent"
                )
            ]
            (List.indexedMap
                (showIndexedColor
                    (guessIndex == 0)
                    (if guessIndex == 0 then
                        model.selectedIndex

                     else
                        -1
                    )
                )
                guess
            )
        , div
            [ style "margin" "0.5em" ]
            [ if guessIndex == 0 then
                button
                    [ onClick NextGuess ]
                    [ text "Lock" ]

              else
                gradeGuess model.secret guess
            ]
        ]


gradeGuess : Cypher -> Cypher -> Html Msg
gradeGuess secret guess =
    text "TBD"


showIndexedColor : Bool -> Int -> Int -> Color -> Html Msg
showIndexedColor clickable selectedIndex i c =
    div
        [ style "display" "inline-block"
        , style "position" "relative"
        , style "border" "1px solid transparent"
        , style "border-color"
            (if i == selectedIndex then
                "black"

             else
                "transparent"
            )
        , style "text-align" "center"
        , style "cursor"
            (if clickable then
                "pointer"

             else
                "inherit"
            )
        , if clickable then
            onClick (SelectIndex i)

          else
            style "opacity" "0.5"
        ]
        [ showColor c ]


showColor : Color -> Html msg
showColor c =
    div
        [ style "display" "inline-block"
        , style "width" "1em"
        , style "height" "1em"
        , style "border" "2px solid black"
        , style "border-radius" "1em"
        , style "background" (colorText c)
        , style "margin" "0.5em"
        ]
        []


showPickableColor : Color -> Html Msg
showPickableColor c =
    div
        [ style "display" "inline-block"
        , style "width" "1.25em"
        , style "height" "1.25em"
        , style "border" "2px solid black"
        , style "border-radius" "1em"
        , style "background" (colorText c)
        , style "margin" "0.5em"
        , onClick (UpdateColor c)
        ]
        []


showColorPicker : Html Msg
showColorPicker =
    div
        [ style "display" "block"
        , style "border" "1px solid black"
        , style "margin-bottom" "1em"
        ]
        (List.map showPickableColor allColors)
