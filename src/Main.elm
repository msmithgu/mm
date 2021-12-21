module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mm exposing (..)
import Random


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
    , games : List Int
    , debug : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { secret = blankCypher
      , guesses = []
      , color = White
      , reveal = False
      , debug = False
      , games = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CheckGuess
    | NewGame
    | SetSecret (List Color)
    | UpdateColor Color
    | UpdateGuessColor Int
    | UpdateGuess Cypher


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckGuess ->
            if gameWon model.guesses model.secret then
                ( { model
                    | reveal = True
                    , games = List.length model.guesses :: model.games
                  }
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

        UpdateGuess g ->
            ( { model | guesses = updateGuess model.guesses 0 g }
            , Cmd.none
            )

        UpdateGuessColor selectedIndex ->
            ( { model | guesses = updateGuessColor model.guesses 0 selectedIndex model.color }
            , Cmd.none
            )



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
        , style "position" "relative"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "font-family" "sans-serif"
        , style "font-size" "24px"
        ]
        [ showSecret model.reveal model.secret
        , showDebug model
        , showScore model.games
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


showScore : List Int -> Html Msg
showScore gameScores =
    div
        [ style "display" "inline-block"
        , style "position" "absolute"
        , style "right" "1em"
        , style "top" "1em"
        ]
        [ div [] [ text ("best: " ++ String.fromInt (bestScore gameScores)) ]
        , div [] [ text ("avrg: " ++ String.fromInt (averageScore gameScores)) ]
        , div [] [ text ("plyd: " ++ String.fromInt (List.length gameScores)) ]
        ]


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
            , div [] [ text "## secret" ]
            , showSecret True model.secret
            , div [] [ text ("color: " ++ colorText model.color) ]
            , div [] [ text ("games: " ++ Debug.toString model.games) ]
            , div [] [ text ("best: " ++ String.fromInt (bestScore model.games)) ]
            , div [] [ text ("avrg: " ++ String.fromInt (averageScore model.games)) ]
            , div [] [ text ("plyd: " ++ String.fromInt (List.length model.games)) ]
            , button [ onClick (UpdateGuess model.secret) ] [ text "Cheat" ]
            ]

    else
        div [] []


showGradeGuess : Cypher -> Cypher -> Html Msg
showGradeGuess secret guess =
    let
        ( numMatching, correctColorsOutOfPosition ) =
            gradeGuess secret guess
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        (List.append (List.repeat numMatching matchPeg) (List.repeat correctColorsOutOfPosition colorPeg))


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
        , style "cursor" "grab"
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
        (List.map (showPickableColor chosenColor) choosableColors)
