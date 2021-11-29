module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import WTFormat exposing (WTNote, parseWTNote, NoteLetter(..), newWTNote, parseLine, Token(..))
import Parser


suite : Test
suite =
  describe "Parse WT"
    [ test "simple note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "d")
        (Ok <| newWTNote D 0 0)
    , test "flat note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "b_")
        (Ok <| newWTNote B -1 0)
    , test "sharp note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "f#")
        (Ok <| newWTNote F 1 0)
    , test "multiple sharp note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "f###")
        (Ok <| newWTNote F 3 0)
    , test "high note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "a+")
        (Ok <| newWTNote A 0 1)
    , test "uppercase note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "G")
        (Ok <| newWTNote G 0 1)
    , test "very high note" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "E+++")
        (Ok <| newWTNote E 0 4)
    , test "combination" <|
      \_ -> Expect.equal
        (Parser.run parseWTNote "B_+")
        (Ok <| newWTNote B -1 2)
    , describe "line"
      [ test "abc" <|
        \_ -> Expect.equal
          (Parser.run parseLine "abc")
          (Ok <|
            [ Note <| newWTNote A 0 0
            , Note <| newWTNote B 0 0
            , Note <| newWTNote C 0 0
            ]
          )
      , test "with spaces" <|
        \_ -> Expect.equal
          (Parser.run parseLine "a b   c ")
          (Ok <|
            [ Note <| newWTNote A 0 0
            , Space
            , Note <| newWTNote B 0 0
            , Space
            , Note <| newWTNote C 0 0
            , Space
            ]
          )
      , test "with slurs" <|
        \_ -> Expect.equal
          (Parser.run parseLine "a-b--c")
          (Ok <|
            [ Note <| newWTNote A 0 0
            , Slur
            , Note <| newWTNote B 0 0
            , Slur
            , Slur
            , Note <| newWTNote C 0 0
            ]
          )
      , test "with bars" <|
        \_ -> Expect.equal
          (Parser.run parseLine "a|b||c")
          (Ok <|
            [ Note <| newWTNote A 0 0
            , Bar
            , Note <| newWTNote B 0 0
            , Bar
            , Bar
            , Note <| newWTNote C 0 0
            ]
          )
      , test "with combinations" <|
        \_ -> Expect.equal
          (Parser.run parseLine "a - b--|c|d#+g  A_-A_")
          (Ok <|
            [ Note <| newWTNote A 0 0
            , Space
            , Slur
            , Space
            , Note <| newWTNote B 0 0
            , Slur
            , Slur
            , Bar
            , Note <| newWTNote C 0 0
            , Bar
            , Note <| newWTNote D 1 1
            , Note <| newWTNote G 0 0
            , Space
            , Note <| newWTNote A -1 1
            , Slur
            , Note <| newWTNote A -1 1
            ]
          )
      ]
    ]
