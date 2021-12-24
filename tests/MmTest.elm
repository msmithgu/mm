module MmTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mm exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Mm module"
        [ describe "gradeGuess"
            [ test "grades trivial fail case correctly" <|
                \_ ->
                    Expect.equal
                        ( 0, 0 )
                        (gradeGuess
                            [ White, White, White, White ]
                            [ Black, Black, Black, Black ]
                        )
            , test "grades trivial win case correctly" <|
                \_ ->
                    Expect.equal
                        ( 4, 0 )
                        (gradeGuess
                            [ White, White, White, White ]
                            [ White, White, White, White ]
                        )
            , test "grades complex case correctly" <|
                \_ ->
                    Expect.equal
                        ( 2, 2 )
                        (gradeGuess
                            [ Red, Green, Green, Green ]
                            [ Green, Red, Green, Green ]
                        )
            , test "grades Mark's case correctly" <|
                \_ ->
                    Expect.equal
                        ( 3, 0 )
                        (gradeGuess
                            [ Pink, Purple, Purple, Red ]
                            [ Purple, Purple, Purple, Red ]
                        )
            , test "grades Kelly's case correctly" <|
                \_ ->
                    Expect.equal
                        ( 0, 4 )
                        (gradeGuess
                            [ Red, Yellow, Red, Green ]
                            [ Yellow, Red, Green, Red ]
                        )
            , test "grades OXXO XOOX case correctly" <|
                \_ ->
                    Expect.equal
                        ( 0, 4 )
                        (gradeGuess
                            [ Green, Red, Red, Green ]
                            [ Red, Green, Green, Red ]
                        )
            , test "grades Dylan's case correctly" <|
                \_ ->
                    Expect.equal
                        ( 1, 1 )
                        (gradeGuess
                            [ Pink, Yellow, Purple, Purple ]
                            [ Yellow, Yellow, Pink, Pink ]
                        )
            , test "grades inverse Dylan's case correctly" <|
                \_ ->
                    Expect.equal
                        ( 1, 1 )
                        (gradeGuess
                            [ Yellow, Yellow, Pink, Pink ]
                            [ Pink, Yellow, Purple, Purple ]
                        )
            , test "grades Mark's late night case correctly" <|
                \_ ->
                    Expect.equal
                        ( 1, 2 )
                        (gradeGuess
                            [ Green, Red, Yellow, Yellow ]
                            [ Red, Green, Yellow, Green ]
                        )
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
