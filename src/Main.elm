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
  , input
  , textarea
  , text
  , ul
  , li
  )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import WTFormat
import Parser
import Note exposing 
  ( RelativeNote(..)
  , AbsoluteNote
  , Letter(..)
  , absolute
  , from
  )



-- MAIN


main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
  { tabText: String
  , base: AbsoluteNote
  , key: AbsoluteNote
  }


init : Model
init = { tabText = "", base = absolute D 4, key = absolute D 4 }


type Finger = O | H | L | X | Z

type alias Fingering = List Finger

parseNote : String -> Maybe RelativeNote
parseNote text = Nothing
  

toFingering : RelativeNote -> Maybe Fingering
toFingering (RelativeNote index) =
  case index of
    0  -> Just [X, X, X, X, X, X]
    1  -> Just [X, X, X, X, X, L]
    2  -> Just [X, X, X, X, X, O]
    3  -> Just [X, X, X, X, L, O]
    4  -> Just [X, X, X, X, O, O]
    5  -> Just [X, X, X, O, O, O]
    6  -> Just [X, X, H, O, O, O]
    7  -> Just [X, X, O, O, O, O]
    8  -> Just [X, H, O, O, O, O]
    9  -> Just [X, O, O, O, O, O]
    10 -> Just [O, X, X, O, O, O]
    11 -> Just [O, O, O, O, O, O]
    12 -> Just [O, X, X, X, X, X]
    13 -> Just [X, X, X, X, X, L]
    14 -> Just [X, X, X, X, X, O]
    15 -> Just [X, X, X, X, L, O]
    16 -> Just [X, X, X, X, O, O]
    17 -> Just [X, X, X, O, O, O]
    18 -> Just [X, X, O, X, X, O]
    19 -> Just [X, X, O, O, O, O]
    20 -> Just [X, O, X, O, O, O]
    21 -> Just [X, O, O, O, O, O]
    22 -> Just [O, X, O, O, O, O]
    23 -> Just [O, O, O, O, O, O]
    _ -> Nothing

-- UPDATE


type Msg = TabChange String


update : Msg -> Model -> Model
update msg model =
  case msg of
      TabChange newTabText -> { model | tabText = newTabText }



-- VIEW

textToNotes : String -> List RelativeNote
textToNotes text =
  text
    |> String.words
    |> List.filterMap parseNote

view : Model -> Html Msg
view model =
  div []
    [ textarea [ value model.tabText, onInput TabChange ] []
    , viewNoteLine model model.tabText
    --, div [] [ text (String.reverse model.tabText) ]
    --, div [] ( [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] |> List.map (\i -> viewNote (RelativeNote i)))
    ]

viewNoteLine : Model -> String -> Html Msg
viewNoteLine model line = Parser.run WTFormat.parseLine line
  |> Result.withDefault []
  |> List.filterMap 
    (\token ->
      case token of
        WTFormat.Note note -> Just
          (
            WTFormat.toAbsolute model.base note 
              |> from model.key
              |> viewNote
          )
        _ -> Nothing
    )
  |> div []

fingerToken : Finger -> String
fingerToken finger =
  case finger of
    O -> "\u{25cb}"
    X -> "\u{25cf}"
    H -> "\u{25d0}"
    L -> "\u{25d1}"
    Z -> "?"

viewNote : RelativeNote -> Html Msg
viewNote note =
  note
    |> toFingering
    |> Maybe.map viewFingering
    |> Maybe.withDefault errorFingering
    
    
viewFinger : Finger -> Html Msg
viewFinger finger =
  li
    [ class "finger" ]
    [ text (fingerToken finger) ]

viewFingering : Fingering -> Html Msg
viewFingering fingering =
  ul
    [ class "tab-note"] 
    (fingering |> List.map viewFinger)

errorFingering : Html Msg
errorFingering =
  ul 
    [ class "tab-note error"]
      ([Z, Z, Z, Z, Z, Z] |> List.map viewFinger)
  
