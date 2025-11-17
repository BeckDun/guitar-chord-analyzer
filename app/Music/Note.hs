module Music.Note
  ( Note(..)
  , noteIndex
  , noteFromIndex
  , interval
  , allNotes
  ) where

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B  -- 's' indicates sharp, i.e C#, D#, etc. 
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

allNotes :: [Note]
allNotes = [minBound .. maxBound]

noteIndex :: Note -> Int
noteIndex = fromEnum

noteFromIndex :: Int -> Note
noteFromIndex i = toEnum (i `mod` 12)

interval :: Note -> Note -> Int
interval root note = (noteIndex note - noteIndex root) `mod` 12