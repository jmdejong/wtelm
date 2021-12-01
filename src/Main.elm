module Main exposing (..)
-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing 
  ( Html
  , Attribute
  , div
  , span
  , input
  , textarea
  , text
  , ul
  , li
  , sup
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import WTFormat
import Parser
import ParserExt
import Note exposing 
  ( RelativeNote(..)
  , WrittenNote(..)
  , Pitch
  , Letter(..)
  , absolutePitch
  , Token(..)
  )



-- MAIN


main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
  { tabText: String
  , base: Pitch
  , key: Pitch
  }


init : Model
init = { tabText = "", base = absolutePitch D 4, key = absolutePitch D 4 }


type Finger = O | H | L | X | Unknown

type alias Fingering = List Finger
  

toFingering : RelativeNote -> (Fingering, Bool)
toFingering (RelativeNote index) =
  case index of
    0  -> ([X, X, X, X, X, X], True)
    1  -> ([X, X, X, X, X, L], True)
    2  -> ([X, X, X, X, X, O], True)
    3  -> ([X, X, X, X, L, O], True)
    4  -> ([X, X, X, X, O, O], True)
    5  -> ([X, X, X, O, O, O], True)
    6  -> ([X, X, H, O, O, O], True)
    7  -> ([X, X, O, O, O, O], True)
    8  -> ([X, H, O, O, O, O], True)
    9  -> ([X, O, O, O, O, O], True)
    10 -> ([O, X, X, O, O, O], True)
    11 -> ([O, O, O, O, O, O], True)
    12 -> ([O, X, X, X, X, X], True)
    13 -> ([X, X, X, X, X, L], True)
    14 -> ([X, X, X, X, X, O], True)
    15 -> ([X, X, X, X, L, O], True)
    16 -> ([X, X, X, X, O, O], True)
    17 -> ([X, X, X, O, O, O], True)
    18 -> ([X, X, H, O, O, O], True)
    19 -> ([X, X, O, O, O, O], True)
    20 -> ([X, O, X, O, O, O], True)
    21 -> ([X, O, O, O, O, O], True)
    22 -> ([O, X, O, O, O, O], True)
    23 -> ([O, O, O, O, O, O], True)
    _  -> (List.repeat 6 Unknown, False)

-- UPDATE


type Msg = TabChange String


update : Msg -> Model -> Model
update msg model =
  case msg of
      TabChange newTabText -> { model | tabText = newTabText }



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ textarea [ value model.tabText, onInput TabChange ] []
    , String.lines model.tabText
      |> List.map (viewNoteLine model)
      |> List.intersperse (div [class "line-break"] [])
      |> div []
    ]

viewNoteLine : Model -> String -> Html Msg
viewNoteLine model line =
  case Parser.run WTFormat.parseLine line of
    Ok tokens -> tokens
      |> List.map (viewToken model.base model.key)
      |> div []
    Err deadEnds -> div [class "invalid-line"] [text <| ParserExt.deadEndsToString deadEnds]


viewToken : Pitch -> Pitch -> Token -> Html Msg
viewToken base key token = 
      case token of
        Note note -> viewNote base key note
        Rest -> span [class "spacer"] []
        Bar -> span [class "bar"] [text "|"]
        Slur -> span [class "slur"] [text "("]

fingerToken : Finger -> String
fingerToken finger =
  case finger of
    O -> "\u{25cb}"
    X -> "\u{25cf}"
    H -> "\u{25d0}"
    L -> "\u{25d1}"
    Unknown -> "?"

viewNote : Pitch -> Pitch -> WrittenNote -> Html Msg
viewNote base key note =
  let
    whistleNote = note |> Note.toAbsolute base |> Note.from key
    (fingering, isKnown) = toFingering whistleNote
  in
  div
    [ class <| if isKnown then "tab-note" else "tab-note error"]
    ((List.map viewFinger fingering) ++ [viewNoteText note])
    

viewNoteText : WrittenNote -> Html Msg
viewNoteText (WrittenNote note) =
  li [ class "tab-note-text" ]
    [ span [class "tab-note-letter"] 
      [ viewLetter note.letter 
        |> (if note.octave > 0 then Char.toUpper else Char.toLower)
        |> String.fromChar
        |> text
      , if note.offset > 0 then
          span [class "sharp"] [text <| String.repeat note.offset "\u{266f}"]
        else if note.offset < 0 then
          span [class "flat"] [text <| String.repeat (-note.offset) "\u{266d}"]
        else
          text ""
      ]
    , sup [class "note-octave"] [ text (String.repeat note.octave "+")]
    ]

viewLetter : Letter -> Char
viewLetter letter = case letter of 
  A -> 'a'
  B -> 'b'
  C -> 'c'
  D -> 'd'
  E -> 'e'
  F -> 'f'
  G -> 'g'
  

viewFinger : Finger -> Html Msg
viewFinger finger =
  li
    [ class "finger" ]
    [ text (fingerToken finger) ]

errorFingering : Html Msg
errorFingering =
  ul 
    [ class "tab-note error"]
      (List.repeat 6 Unknown |> List.map viewFinger)


