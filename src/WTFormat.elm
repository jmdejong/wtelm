module WTFormat exposing 
  ( WTNote
  , parseWTNote
  , newWTNote
  , Token(..)
  , parseLine
  , toAbsolute
  )

import Parser exposing 
  ( Parser
  , (|.)
  , (|=)
  , succeed
  , problem
  , symbol
  , float
  , spaces
  , getChompedString
  , chompIf
  , chompWhile
  , map
  , andThen
  , oneOf
  , token
  , loop
  , Step(..)
  , end
  )
import Note exposing 
  ( AbsoluteNote
  , Letter(..)
  , RelativeNote(..)
  , absoluteNext
  , add
  )

type Token = Note WTNote | Space | Slur | Bar

type WTNote = WTNote {letter: Letter, offset: Int, octave: Int}


type alias ParsedNote = {letter: {letter: Letter, isUpper: Bool}, sharps: Int, flats: Int, octaves: Int}

parseLine : Parser (List Token)
parseLine =
  loop [] (\state ->
    oneOf
      [ succeed (\s -> Loop (s :: state))
        |= parseToken
      , token "\n" |> map (\_ -> Done (List.reverse state))
      , end |> map (\_ -> Done (List.reverse state))
      ]
  )

parseToken : Parser Token
parseToken =
  oneOf
    [ succeed Space
      |. token " "
      |. spaces
    , succeed Slur
      |. token "-"
    , succeed Bar
      |. token "|"
    , succeed Note
      |= parseWTNote
    ]

noteLetterFromChar : Char -> Maybe Letter
noteLetterFromChar c =
  case Char.toLower c of
    'a' -> Just A
    'b' -> Just B
    'c' -> Just C
    'd' -> Just D
    'e' -> Just E
    'f' -> Just F
    'g' -> Just G
    _ -> Nothing

newWTNote : Letter -> Int -> Int -> WTNote
newWTNote letter offset octave =
  WTNote
    { letter = letter
    , offset = offset
    , octave = octave
    }

toAbsolute : AbsoluteNote -> WTNote -> AbsoluteNote
toAbsolute base (WTNote note) =
  absoluteNext base note.letter
    |> add (RelativeNote <| 12 * note.octave + note.offset)

writtenNoteFromParsedNote : ParsedNote -> WTNote
writtenNoteFromParsedNote parsedNote =
  WTNote
    { letter = parsedNote.letter.letter
    , offset = parsedNote.sharps - parsedNote.flats
    , octave = parsedNote.octaves + if parsedNote.letter.isUpper then 1 else 0
    }


parseWTNote: Parser WTNote
parseWTNote =
  parseNote
    |> map writtenNoteFromParsedNote

parseLetter : Parser {letter: Letter, isUpper: Bool}
parseLetter = 
  chompIf Char.isAlpha
    |> getChompedString
    |> andThen (\s ->
      case String.uncons s of
        Just (c, "") -> succeed c
        _ -> problem "Note letter should be a single character"
      )
    |> andThen (\c ->
      case noteLetterFromChar c of
        Just letter -> succeed {letter = letter, isUpper = Char.isUpper c}
        Nothing -> problem "Note letter must be between A and G"
      )

countChar : Char -> Parser Int
countChar c =
  chompWhile ((==) c) 
    |> getChompedString
    |> map String.length
    
parseNote : Parser ParsedNote
parseNote = 
  succeed ParsedNote
    |= parseLetter
    |= countChar '#'
    |= countChar '_'
    |= countChar '+'
