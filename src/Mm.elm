module Mm exposing (..)

import List.Extra
import Random



-- MODEL


blankCypher : Cypher
blankCypher =
    List.repeat 4 White


choosableColors : List Color
choosableColors =
    [ Purple
    , Pink
    , Red
    , Green
    , Blue
    , Yellow
    ]


type Color
    = White
    | Black
    | Purple
    | Pink
    | Red
    | Green
    | Blue
    | Yellow


type alias Cypher =
    List Color


colorText : Color -> String
colorText c =
    case c of
        Purple ->
            "purple"

        Pink ->
            "pink"

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


textToColor : String -> Color
textToColor c =
    case c of
        "purple" ->
            Purple

        "pink" ->
            Pink

        "red" ->
            Red

        "green" ->
            Green

        "blue" ->
            Blue

        "yellow" ->
            Yellow

        "white" ->
            White

        "black" ->
            Black

        _ ->
            White



-- UPDATE


gameWon : List Cypher -> Cypher -> Bool
gameWon guessList secret =
    case guessList of
        [] ->
            False

        guess :: _ ->
            List.Extra.isPrefixOf secret guess


updateGuess : List Cypher -> Int -> Cypher -> List Cypher
updateGuess guessList selectedGuessIndex g =
    let
        guessUpdate i guess =
            if i == selectedGuessIndex then
                g

            else
                guess
    in
    List.indexedMap guessUpdate guessList


updateGuessColor : List Cypher -> Int -> Int -> Color -> List Cypher
updateGuessColor guessList selectedGuessIndex selectedColorIndex col =
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
    Random.uniform
        Purple
        [ Pink
        , Red
        , Green
        , Blue
        , Yellow
        ]



-- UTIL


listEqual : List a -> List a -> Bool
listEqual =
    List.Extra.isPrefixOf


equal : a -> a -> Bool
equal a b =
    a == b


pairEqual : ( a, a ) -> Bool
pairEqual ( a, b ) =
    a == b


pairNotEqual : ( a, a ) -> Bool
pairNotEqual ( a, b ) =
    a /= b


sortColors : List Color -> List Color
sortColors colors =
    colors
        |> List.map colorText
        |> List.sort
        |> List.map textToColor


colorReps : List Color -> List ( Color, Int )
colorReps colors =
    colors
        |> sortColors
        |> List.Extra.group
        |> List.map
            (\( c, reps ) ->
                ( c, 1 + List.length reps )
            )


repsToList : ( a, Int ) -> List a
repsToList ( foo, n ) =
    List.repeat n foo


matchMinRep : ( a, Int ) -> ( a, Int ) -> ( a, Int )
matchMinRep ( v1, r1 ) ( v2, r2 ) =
    if v1 == v2 then
        if r1 < r2 then
            ( v1, r1 )

        else
            ( v1, r2 )

    else
        ( v1, 0 )


intersectColors : List Color -> List Color -> List Color
intersectColors a b =
    let
        aReps =
            colorReps a

        bReps =
            colorReps b
    in
    aReps
        |> List.concatMap (\rep -> List.map (matchMinRep rep) bReps)
        |> List.concatMap repsToList
        |> List.foldl (::) []


gradeGuess : Cypher -> Cypher -> ( Int, Int )
gradeGuess secret guess =
    let
        z =
            List.Extra.zip (List.map colorText guess) (List.map colorText secret)

        numMatching =
            List.length (List.filter pairEqual z)

        ( unmatchedGuessElements, unmatchedSecretElements ) =
            List.unzip (List.filter pairNotEqual z)

        correctColorsOutOfPosition =
            List.length
                (intersectColors
                    (List.map textToColor unmatchedGuessElements)
                    (List.map textToColor unmatchedSecretElements)
                )
    in
    ( numMatching, correctColorsOutOfPosition )


bestScore : List Int -> Int
bestScore gameScores =
    case List.minimum gameScores of
        Nothing ->
            0

        Just n ->
            n


averageScore : List Int -> Int
averageScore gameScores =
    List.sum gameScores // List.length gameScores
