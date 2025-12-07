module Music.Frets
  ( Fret
  , ChordShape
  , standardTuning
  , standardTuningPitched
  , noteAt
  , pitchedNoteAt
  , notesFromShape
  , pitchedNotesFromShape
  , recognizeChordFromShape
  ) where

import Music.Note
import Music.Chord

type Fret = Maybe Int
type ChordShape = [Fret]

standardTuning :: [Note]
standardTuning = [E, A, D, G, B, E]

-- Open string pitches: E2, A2, D3, G3, B3, E4
standardTuningPitched :: [PitchedNote]
standardTuningPitched =
  [ PitchedNote E 2   -- String 6 (low E)
  , PitchedNote A 2   -- String 5
  , PitchedNote D 3   -- String 4
  , PitchedNote G 3   -- String 3
  , PitchedNote B 3   -- String 2
  , PitchedNote E 4   -- String 1 (high E)
  ]

noteAt :: Note -> Int -> Note
noteAt openNote fret = noteFromIndex (noteIndex openNote + fret)

pitchedNoteAt :: PitchedNote -> Int -> PitchedNote
pitchedNoteAt openNote fret = 
  pitchedNoteFromIndex (pitchedNoteToIndex openNote + fret)

notesFromShape :: [Note] -> ChordShape -> [Note]
notesFromShape tuning frets =
  [ noteAt openNote fret
  | (openNote, Just fret) <- zip tuning frets
  ]

pitchedNotesFromShape :: [PitchedNote] -> ChordShape -> [PitchedNote]
pitchedNotesFromShape tuning frets =
  [ pitchedNoteAt openNote fret
  | (openNote, Just fret) <- zip tuning frets
  ]

recognizeChordFromShape :: ChordShape -> Maybe (Note, String)
recognizeChordFromShape shape =
  identifyChord (notesFromShape standardTuning shape)