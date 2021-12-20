module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (zip)
import Random
import Set
import Tuple exposing (first, second)



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
    , color : Color
    , reveal : Bool
    , debug : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { secret = blankCypher
      , guesses = []
      , color = Black
      , reveal = False
      , debug = False
      }
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
    = CheckGuess
    | NewGame
    | SetSecret (List Color)
    | UpdateColor Color
    | UpdateGuessColor Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckGuess ->
            if gameWon model then
                ( { model | reveal = True }
                , Cmd.none
                )

            else
                ( { model | guesses = blankCypher :: model.guesses }
                , Cmd.none
                )

        NewGame ->
            ( { model | guesses = [ blankCypher ], reveal = False }
            , Random.generate SetSecret (colorListGenerator 4)
            )

        SetSecret s ->
            ( { model | secret = s }
            , Cmd.none
            )

        UpdateColor c ->
            ( { model | color = c }
            , Cmd.none
            )

        UpdateGuessColor selectedIndex ->
            ( { model | guesses = updateGuess model.guesses 0 selectedIndex model.color }
            , Cmd.none
            )


gameWon : Model -> Bool
gameWon model =
    case model.guesses of
        [] ->
            False

        guess :: _ ->
            List.Extra.isPrefixOf model.secret guess


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
        , style "font-size" "24px"
        ]
        [ showSecret model.reveal model.secret
        , showDebug model
        , div [ style "margin" "1em" ]
            [ button
                [ onClick NewGame ]
                [ text "New Game" ]
            ]
        , showColorPalette model.color
        , showGuesses model
        ]


showSecret : Bool -> Cypher -> Html Msg
showSecret reveal secret =
    if reveal then
        div [] (List.map showColor secret)

    else
        div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "justify-content" "space-between"
            , style "width" "7em"
            ]
            (List.repeat
                4
                (div [ style "display" "inline-block" ] [ text "?" ])
            )


showDebug : Model -> Html Msg
showDebug model =
    if model.debug then
        div
            [ style "border" "1px dashed #0f0"
            , style "margin" "1em"
            , style "padding" "0.5em 1em"
            , style "background" "black"
            , style "color" "#0f0"
            , style "font-family" "monospace"
            ]
            [ div [] [ text "# debug" ]
            , div [] [ text ("color: " ++ colorText model.color) ]
            ]

    else
        div [] []


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
                )
                guess
            )
        , div
            [ style "margin" "0.5em" ]
            [ showGuessStatus model guessIndex guess ]
        ]


showGuessStatus : Model -> Int -> Cypher -> Html Msg
showGuessStatus model guessIndex guess =
    case ( model.reveal, guessIndex ) of
        ( gameOver, 0 ) ->
            if gameOver then
                showGradeGuess model.secret guess

            else
                button
                    [ onClick CheckGuess ]
                    [ text "Guess" ]

        _ ->
            showGradeGuess model.secret guess


listEqual : List a -> List a -> Bool
listEqual =
    List.Extra.isPrefixOf


pairEqual : ( a, a ) -> Bool
pairEqual ( a, b ) =
    a == b


pairNotEqual : ( a, a ) -> Bool
pairNotEqual ( a, b ) =
    a /= b


showGradeGuess : Cypher -> Cypher -> Html Msg
showGradeGuess secret guess =
    let
        z =
            zip guess secret

        numMatching =
            List.length (List.filter pairEqual z)

        notMatching =
            List.unzip (List.filter pairNotEqual z)

        correctColorsOutOfPosition =
            Set.size
                (Set.intersect
                    (Set.fromList (List.map colorText (Tuple.first notMatching)))
                    (Set.fromList (List.map colorText (Tuple.second notMatching)))
                )
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        (List.append (List.repeat numMatching matchPeg) (List.repeat correctColorsOutOfPosition colorPeg))


matchPeg : Html Msg
matchPeg =
    div
        [ style "width" "0.5em"
        , style "height" "0.5em"
        , style "border" "2px solid black"
        , style "border-radius" "0.5em"
        , style "background" "black"
        , style "margin" "0.25em"
        ]
        []


colorPeg : Html Msg
colorPeg =
    div
        [ style "width" "0.5em"
        , style "height" "0.5em"
        , style "border" "2px solid black"
        , style "border-radius" "0.5em"
        , style "background" "white"
        , style "margin" "0.25em"
        ]
        []


showIndexedColor : Bool -> Int -> Color -> Html Msg
showIndexedColor clickable i c =
    div
        [ style "display" "inline-block"
        , style "text-align" "center"
        , style "cursor"
            (if clickable then
                "pointer"

             else
                "inherit"
            )
        , if clickable then
            onClick (UpdateGuessColor i)

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


showPickableColor : Color -> Color -> Html Msg
showPickableColor chosenColor c =
    let
        size =
            if c == chosenColor then
                "1.75em"

            else
                "1.25em"
    in
    div
        [ style "display" "inline-block"
        , style "width" size
        , style "height" size
        , style "border" "2px solid black"
        , style "border-radius" "1em"
        , style "background" (colorText c)
        , style "margin"
            (if c == chosenColor then
                "0.25em"

             else
                "0.5em"
            )
        , onClick (UpdateColor c)
        ]
        []


showColorPalette : Color -> Html Msg
showColorPalette chosenColor =
    div
        [ style "display" "block"
        , style "border" "1px solid black"
        , style "margin-bottom" "1em"
        ]
        (List.map (showPickableColor chosenColor) allColors)
