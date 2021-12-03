module ParserExt exposing 
  ( deadEndsToString
  , repeat
  , prefixedLine
  , countChar
  )

import Parser exposing
  ( DeadEnd
  , Problem(..)
  , Parser
  , map
  , Step(..)
  , oneOf
  , loop
  , chompUntilEndOr
  , getChompedString
  , (|=)
  , (|.)
  , token
  , succeed
  , chompWhile
  )

deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
  String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend = 
  problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String 
problemToString p = 
  case p of 
    Expecting s -> "expecting '" ++ s ++ "'"
    ExpectingInt -> "expecting int" 
    ExpectingHex -> "expecting hex" 
    ExpectingOctal -> "expecting octal" 
    ExpectingBinary -> "expecting binary" 
    ExpectingFloat -> "expecting float" 
    ExpectingNumber -> "expecting number" 
    ExpectingVariable -> "expecting variable" 
    ExpectingSymbol s -> "expecting symbol '" ++ s ++ "'"
    ExpectingKeyword s -> "expecting keyword '" ++ s ++ "'"
    ExpectingEnd -> "expecting end" 
    UnexpectedChar -> "unexpected char" 
    Problem s -> "problem " ++ s 
    BadRepeat -> "bad repeat" 


repeat : Parser () -> Parser a -> Parser (List a)
repeat final continue = 
  loop [] (\state ->
    oneOf
      [ final |> map (\_ -> Done (List.reverse state))
      , continue |> map (\s -> Loop (s :: state))
      ]
  )

prefixedLine : String -> Parser String
prefixedLine prefix = succeed identity
  |. token prefix
  |= getChompedString (chompUntilEndOr "\n")


countChar : Char -> Parser Int
countChar c =
  chompWhile ((==) c) 
    |> getChompedString
    |> map String.length
