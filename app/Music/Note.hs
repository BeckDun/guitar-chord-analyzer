module Music.Note
  ( Note(..)
  , PitchedNote(..)
  , noteIndex
  , noteFromIndex
  , interval
  , allNotes
  , pitchedNoteToIndex
  , pitchedNoteFromIndex
  ) where

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data PitchedNote = PitchedNote Note Int  -- Note and octave
  deriving (Eq, Ord, Show, Read)

allNotes :: [Note]
allNotes = [minBound .. maxBound]

noteIndex :: Note -> Int
noteIndex = fromEnum

noteFromIndex :: Int -> Note
noteFromIndex i = toEnum (i `mod` 12)

interval :: Note -> Note -> Int
interval root note = (noteIndex note - noteIndex root) `mod` 12

-- Convert PitchedNote to absolute semitone index (C0 = 0)
pitchedNoteToIndex :: PitchedNote -> Int
pitchedNoteToIndex (PitchedNote note octave) = octave * 12 + noteIndex note

-- Convert absolute semitone index to PitchedNote
pitchedNoteFromIndex :: Int -> PitchedNote
pitchedNoteFromIndex i = PitchedNote (noteFromIndex i) (i `div` 12)