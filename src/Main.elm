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
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
import WTFormat
import Parser
import ParserExt
import Note exposing 
  ( Letter(..)
  , absolutePitch
  )
import Tab exposing ( TabSettings )
import TabRenderer exposing ( renderTabLine )



-- MAIN


main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
  { tabText: String
  , tabSettings: TabSettings
  }


init : Model
init = 
  { tabText = ""
  , tabSettings = 
    { base = absolutePitch D 5
    , key = absolutePitch D 5
    }
  }


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
    , renderTab model.tabSettings model.tabText
    ]

    
renderTab : TabSettings -> String -> Html msg
renderTab settings tabText =
  String.lines tabText
    |> List.map (viewNoteLine settings)
    |> List.intersperse (div [class "line-break"] [])
    |> div []


viewNoteLine : TabSettings -> String -> Html msg
viewNoteLine settings line =
  case Parser.run WTFormat.parseLine line of
    Ok tokens -> renderTabLine settings tokens
    Err deadEnds -> div [class "invalid-line"] [text <| ParserExt.deadEndsToString deadEnds]


