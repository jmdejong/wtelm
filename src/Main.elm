module Main exposing (..)

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
import TabRenderer exposing ( renderTab )



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
    , viewTab model.tabSettings model.tabText
    ]

    
viewTab : TabSettings -> String -> Html msg
viewTab settings tabText =
  case Parser.run WTFormat.parseTabText tabText of 
    Ok lines -> renderTab settings lines
    Err deadEnds -> div [class "invalid-line"] [text <| ParserExt.deadEndsToString deadEnds]


