module MmTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mm exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Mm module"
        [ describe "intersectColors"
            [ test "empty first argument" <|
                \_ ->
                    Expect.equal
                        (intersectColors [] [ Red, Green, Blue ])
                        []
            , test "empty second argument" <|
                \_ ->
                    Expect.equal
                        (intersectColors [ Red, Green, Blue ] [])
                        []
            , test "1st element match" <|
                \_ ->
                    Expect.equal
                        (intersectColors [ Red, Green, Blue ] [ Red, Purple, Yellow ])
                        [ Red ]
            , test "middle element match" <|
                \_ ->
                    Expect.equal
                        (intersectColors [ Red, Purple, Blue ] [ Yellow, Purple, Pink ])
                        [ Purple ]
            , test "Trader Joe case" <|
                \_ ->
                    Expect.equal
                        (intersectColors
                            [ Red, Green, Red, Red ]
                            [ Red, Red, Green, Green ]
                        )
                        [ Red, Red, Green ]
            ]
        , describe "gradeGuess"
            [ test "grades trivial fail case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ White, White, White, White ]
                            [ Black, Black, Black, Black ]
                        )
                        ( 0, 0 )
            , test "grades trivial win case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ White, White, White, White ]
                            [ White, White, White, White ]
                        )
                        ( 4, 0 )
            , test "grades complex case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Red, Green, Green, Green ]
                            [ Green, Red, Green, Green ]
                        )
                        ( 2, 2 )
            , test "grades Mark's case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Pink, Purple, Purple, Red ]
                            [ Purple, Purple, Purple, Red ]
                        )
                        ( 3, 0 )
            , test "grades Kelly's case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Red, Yellow, Red, Green ]
                            [ Yellow, Red, Green, Red ]
                        )
                        ( 0, 4 )
            , test "grades OXXO XOOX case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Green, Red, Red, Green ]
                            [ Red, Green, Green, Red ]
                        )
                        ( 0, 4 )
            , test "grades Dylan's case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Pink, Yellow, Purple, Purple ]
                            [ Yellow, Yellow, Pink, Pink ]
                        )
                        ( 1, 1 )
            , test "grades inverse Dylan's case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Yellow, Yellow, Pink, Pink ]
                            [ Pink, Yellow, Purple, Purple ]
                        )
                        ( 1, 1 )
            , test "grades Mark's late night case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Green, Red, Yellow, Yellow ]
                            [ Red, Green, Yellow, Green ]
                        )
                        ( 1, 2 )
            , test "grades Trader Joe case correctly" <|
                \_ ->
                    Expect.equal
                        (gradeGuess
                            [ Red, Green, Red, Red ]
                            [ Red, Red, Green, Green ]
                        )
                        ( 1, 2 )
            ]
        ]
