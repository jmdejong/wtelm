module WTFormat exposing ( parseTabText )

import Parser exposing 
  ( Parser
  , (|.)
  , (|=)
  , succeed
  , problem
  , float
  , spaces
  , getChompedString
  , chompIf
  , map
  , andThen
  , oneOf
  , token
  , end
  )
import ParserExt exposing
  ( prefixedLine
  , repeat
  , countChar
  )
import Note
import Note exposing 
  ( Letter(..)
  , WrittenNote(..)
  )
import Tab exposing (Token(..), Line(..))



type alias ParsedNote = {letter: {letter: Letter, isUpper: Bool}, sharps: Int, flats: Int, octaves: Int}

parseTabText : Parser (List Line)
parseTabText =
  repeat end <|
    oneOf
      [ prefixedLine "---"
        |> map Heading
      , prefixedLine "--"
        |> map (String.split " ")
        |> map Lyrics
      , prefixedLine "-"
        |> map Comment
      , parseNoteLine 
        |> map Notes
      ]

  



parseNoteLine : Parser (List Token)
parseNoteLine =
  repeat (oneOf [token "\n", end]) parseToken

parseToken : Parser Token
parseToken =
  oneOf
    [ succeed Rest
      |. token " "
      |. spaces
    , succeed Slur
      |. token "-"
    , succeed Bar
      |. token "|"
    , succeed Note
      |= parseWrittenNote
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

writtenNoteFromParsedNote : ParsedNote -> WrittenNote
writtenNoteFromParsedNote parsedNote =
  WrittenNote
    { letter = parsedNote.letter.letter
    , offset = parsedNote.sharps - parsedNote.flats
    , octave = parsedNote.octaves + if parsedNote.letter.isUpper then 1 else 0
    }


parseWrittenNote: Parser WrittenNote
parseWrittenNote =
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
    
parseNote : Parser ParsedNote
parseNote = 
  succeed ParsedNote
    |= parseLetter
    |= countChar '#'
    |= countChar '_'
    |= countChar '+'
