module TabRenderer exposing 
  ( renderTab
  , viewLetter
  )

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
  , p
  )
import Html.Attributes exposing (class)
import Tab exposing
  ( TabSettings
  , Token(..)
  , Line(..)
  , baseKey
  )
import Note exposing 
  ( RelativeNote(..)
  , WrittenNote(..)
  , Pitch
  , Letter(..)
  )

renderTab : TabSettings -> List Line -> Html msg
renderTab settings lines =
  lines
    |> List.map (renderTabLine settings)
    |> List.intersperse (div [class "line-break"] [])
    |> div []

renderTabLine : TabSettings -> Line -> Html msg
renderTabLine settings line = case line of
  Notes notes -> div [] (List.map (viewToken settings) notes )
  Lyrics lyrics -> lyrics
    |> List.map viewLyricWord
    |> p [ class "comment lyric" ]
  Comment comment -> p [ class "comment text"] [text comment]
  Heading heading -> p [ class "comment heading"] [text heading]
  Empty -> text ""

viewLyricWord : String -> Html msg
viewLyricWord word =
  span
    [class "spacing"]
    [ span
        [class "word"]
        [text word]
    ]

viewToken : TabSettings -> Token -> Html msg
viewToken settings token = 
      case token of
        Note note -> viewNote (baseKey settings) settings.key note
        Rest -> span [class "spacer"] []
        Bar -> span [class "bar"] [text "|"]
        Slur -> span [class "slur"] [text "("]

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

fingerToken : Finger -> String
fingerToken finger =
  case finger of
    O -> "\u{25cb}"
    X -> "\u{25cf}"
    H -> "\u{25d0}"
    L -> "\u{25d1}"
    Unknown -> "?"

viewNote : Pitch -> Pitch -> WrittenNote -> Html msg
viewNote base key note =
  let
    whistleNote = note |> Note.toAbsolute base |> Note.from key
    (fingering, isKnown) = toFingering whistleNote
  in
  div
    [ class <| if isKnown then "tab-note" else "tab-note error"]
    ((List.map viewFinger fingering) ++ [viewNoteText note])
    

viewNoteText : WrittenNote -> Html msg
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
  A -> 'A'
  B -> 'B'
  C -> 'C'
  D -> 'D'
  E -> 'E'
  F -> 'F'
  G -> 'G'
  

viewFinger : Finger -> Html msg
viewFinger finger =
  li
    [ class "finger" ]
    [ text (fingerToken finger) ]

errorFingering : Html msg
errorFingering =
  ul 
    [ class "tab-note error"]
      (List.repeat 6 Unknown |> List.map viewFinger)
