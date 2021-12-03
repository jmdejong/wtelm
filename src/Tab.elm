module Tab exposing
  ( Token(..)
  , TabSettings
  , Line(..)
  , Base(..)
  , baseKey
  )

import Note exposing
  ( WrittenNote
  , Pitch
  )

type Base = CustomBase Pitch | SameAsKey

type alias TabSettings = {base: Base, key: Pitch}

baseKey : TabSettings -> Pitch
baseKey settings = case settings.base of
  SameAsKey -> settings.key
  CustomBase pitch -> pitch

type Token = Note WrittenNote | Rest | Slur | Bar

type alias Tab = List (List Token)

type Line = Notes (List Token) | Comment String | Heading String | Lyrics (List String) | Empty
