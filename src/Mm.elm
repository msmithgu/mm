module Mm exposing (..)

import List.Extra exposing (zip)
import Random
import Set



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


pairEqual : ( a, a ) -> Bool
pairEqual ( a, b ) =
    a == b


pairNotEqual : ( a, a ) -> Bool
pairNotEqual ( a, b ) =
    a /= b


gradeGuess : Cypher -> Cypher -> ( Int, Int )
gradeGuess secret guess =
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
