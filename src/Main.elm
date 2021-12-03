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
  , label
  , select
  , option
  )
import Html.Attributes exposing 
  ( class
  , value
  , id
  , selected
  )
import Html.Events exposing (onInput)
import WTFormat
import Parser
import ParserExt
import Note exposing 
  ( Letter(..)
  , absolutePitch
  , toWritten
  , WrittenNote(..)
  , Pitch
  , RelativeNote(..)
  , add
  )
import Tab exposing
  ( TabSettings
  , Base(..)
  )
import TabRenderer exposing
  ( renderTab
  , viewLetter
  )



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
    { base = SameAsKey
    , key = absolutePitch D 5
    }
  }


-- UPDATE


type Msg 
  = TabChange String
  | KeyChange (Maybe Pitch)
  | BaseChange (Maybe Base)


update : Msg -> Model -> Model
update msg model =
  case msg of
      TabChange newTabText -> { model | tabText = newTabText }
      KeyChange (Just key) -> { model | tabSettings = {key = key, base = model.tabSettings.base}}
      KeyChange Nothing -> model
      BaseChange (Just base) -> { model | tabSettings = {key = model.tabSettings.key, base = base}}
      BaseChange Nothing -> model



-- VIEW

view : Model -> Html Msg
view model =
  let
    pitchFromCode i = add (RelativeNote i) (absolutePitch C 4)
  in
  div []
    [ textarea [ value model.tabText, onInput TabChange ] []
    , div [id "tab"] [viewTab model.tabSettings model.tabText]
    , label [] 
      [ text "Whistle key:"
      , select 
        [ id "whistle-key"
        , onInput (String.toInt >> Maybe.map pitchFromCode >> KeyChange)
        ]
        <|
          List.map 
            (\n ->
              let
                pitch = pitchFromCode n
              in
              option 
                [ selected (pitch == model.tabSettings.key)
                , value <| String.fromInt n
                ]
                [text <| showPitch pitch]
            )
            (List.range 0 24)
      ]
    , text " "
    , label [] 
      [ text "Octave base:"
      , select 
        [ id "octave-base"
        , onInput <|
            \b -> BaseChange <| if b == "s" then
                Just SameAsKey
              else
                String.toInt b |> Maybe.map (pitchFromCode >> CustomBase)
        ]
        <| (option [selected (model.tabSettings.base == SameAsKey), value "s"] [text "Same as key"])
          :: List.map 
            (\n ->
              let
                pitch = pitchFromCode n
              in
              option 
                [ selected (CustomBase pitch == model.tabSettings.base)
                , value <| String.fromInt n
                ]
                [text <| showPitch pitch]
            )
            (List.range 0 24)
      ]
    ]

showPitch : Pitch -> String
showPitch pitch = toWritten pitch
  |> List.map (\(WrittenNote note) -> 
      String.concat 
        [ String.fromChar <| viewLetter note.letter
        , if note.offset > 0 then
            String.repeat note.offset "\u{266f}"
          else if note.offset < 0 then
            String.repeat (-note.offset) "\u{266d}"
          else
            ""
        , String.fromInt note.octave
        ]
      )
  |> String.join "/"


viewTab : TabSettings -> String -> Html msg
viewTab settings tabText =
  case Parser.run WTFormat.parseTabText tabText of 
    Ok lines -> renderTab settings lines
    Err deadEnds -> div [class "invalid-line"] [text <| ParserExt.deadEndsToString deadEnds]


