module Tab exposing
  ( Token(..)
  , TabSettings
  , Line(..)
  )

import Note exposing
  ( WrittenNote
  , Pitch
  )

type alias TabSettings = {base: Pitch, key: Pitch}

type Token = Note WrittenNote | Rest | Slur | Bar

type alias Tab = List (List Token)

type Line = Notes (List Token) | Comment String | Heading String | Lyrics (List String) | Empty
