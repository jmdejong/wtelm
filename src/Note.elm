module Note exposing
  ( Pitch
  , absolutePitch
  , toAbsolute
  , from
  , add
  , Letter(..)
  , RelativeNote(..)
  , WrittenNote(..)
  , toWritten
  )

type Pitch = Pitch Int

absolutePitch : Letter -> Int -> Pitch
absolutePitch letter octave = Pitch <|
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

absoluteNext : Pitch -> Letter -> Pitch
absoluteNext (Pitch code) letter = 
  (letterToCode letter) - code
    |> modBy 12
    |> (+) code
    |> Pitch

toWritten : Pitch -> List WrittenNote
toWritten (Pitch code) =
  let
    n letter offset =
      WrittenNote 
        { letter = letter
        , offset = offset
        , octave = (code - offset) // 12
        }
    sf s f = [(n s 1), (n f -1)]
    o l = [(n l 0)]
  in
  case modBy 12 code of
    0 -> o C
    1 -> sf C D
    2 -> o D
    3 -> sf D E
    4 -> o E
    5 -> o F
    6 -> sf F G
    7 -> o G
    8 -> sf G A
    9 -> o A
    10 -> sf A B
    11 -> o B
    _ -> o C -- should never occur because of the modBy 12

    
--toLetter : Bool -> Pitch -> (Letter, Int)
--toLetter isFlat pitch =
  --lettersByPitch pitch
    --|> if isFlat then Tuple.second else Tuple.first

--toWritten : Pitch -> [WrittenNote]
--toWritten pitch =
  --lettersByPitch pitch
    --|> ((letter, offset) -> 
  --let
    --(letter, offset) = toLetter isFlat pitch 
    --(RelativeNote difference) = from key pitch
    --octave = difference // 12
  --in
  --WrittenNote {letter = letter, offset = offset, octave = octave}

type RelativeNote = RelativeNote Int

add : RelativeNote -> Pitch -> Pitch
add (RelativeNote relativeCode) (Pitch absoluteCode) =
  Pitch (absoluteCode + relativeCode)

from : Pitch -> Pitch -> RelativeNote
from (Pitch baseCode) (Pitch noteCode) =
  RelativeNote (noteCode - baseCode)

type alias AbsoluteNote = {pitch: Pitch, appliedOffset: Int}

type WrittenNote = WrittenNote {letter: Letter, offset: Int, octave: Int}

toAbsolute : Pitch -> WrittenNote -> Pitch
toAbsolute base (WrittenNote note) =
  absoluteNext base note.letter
    |> add (RelativeNote <| 12 * note.octave + note.offset)

