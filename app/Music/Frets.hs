module Music.Frets
  ( Fret
  , ChordShape
  , standardTuning
  , noteAt
  , notesFromShape
  ) where

import Music.Note
import Music.Chord


{-
Uses the guitar tuning to identify selected notes, then identifies those notes as a chord
-}
type Fret = Maybe Int
type ChordShape = [Fret]

standardTuning :: [Note]
standardTuning = [E, A, D, G, B, E]

noteAt :: Note -> Int -> Note
noteAt openNote fret = noteFromIndex (noteIndex openNote + fret)

notesFromShape :: [Note] -> ChordShape -> [Note]
notesFromShape tuning frets =
  [ noteAt openNote fret
  | (openNote, Just fret) <- zip tuning frets
  ]

recognizeChordFromShape :: ChordShape -> Maybe (Note, String)
recognizeChordFromShape shape =
  identifyChord (notesFromShape standardTuning shape)