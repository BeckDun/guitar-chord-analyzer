module Music.Chord
  ( chordPatterns
  , identifyChord
  ) where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import Music.Note

{-
List of most common chord patterns -- Max
-}
chordPatterns :: [(String, [Int])]
chordPatterns =
  [ ("maj7", [0,4,7,11])
  , ("min7", [0,3,7,10])
  , ("dom7", [0,4,7,10])
  , ("maj",  [0,4,7])
  , ("min",  [0,3,7])
  , ("dim",  [0,3,6])
  , ("aug",  [0,4,8])
  ]


{-
Given a list of notes, selects the chord from the list above that best matches the note intervals from the root -- Max
-}
identifyChord :: [Note] -> Maybe (Note, String)
identifyChord notes = listToMaybe
  [ (root, name)
  | root <- notes
  , let intervals = sort [interval root n | n <- notes]
  , (name, pattern) <- chordPatterns
  , all (`elem` intervals) pattern
  ]