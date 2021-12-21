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
            ]
        ]
