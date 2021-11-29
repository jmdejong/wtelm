module Note exposing
  ( AbsoluteNote
  , absolute
  , absoluteNext
  , add
  , from
  , Letter(..)
  , RelativeNote(..)
  )

type AbsoluteNote = AbsoluteNote Int

absolute : Letter -> Int -> AbsoluteNote
absolute letter octave = AbsoluteNote <|
  letterToCode letter + 12 * octave

type Letter = A | B | C | D | E | F | G


letterToCode : Letter -> Int
letterToCode letter =
  case letter of
    C -> 0
    D -> 2
    E -> 4
    F -> 5
    G -> 7
    A -> 9
    B -> 11

absoluteNext : AbsoluteNote -> Letter -> AbsoluteNote
absoluteNext (AbsoluteNote code) letter = 
  (letterToCode letter) - code
    |> modBy 12
    |> (+) code
    |> AbsoluteNote

type RelativeNote = RelativeNote Int

add : RelativeNote -> AbsoluteNote -> AbsoluteNote
add (RelativeNote relativeCode) (AbsoluteNote absoluteCode) =
  AbsoluteNote (absoluteCode + relativeCode)

from : AbsoluteNote -> AbsoluteNote -> RelativeNote
from (AbsoluteNote baseCode) (AbsoluteNote noteCode) =
  RelativeNote (noteCode - baseCode)
