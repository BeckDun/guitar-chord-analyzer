
import Music.Note
import Music.Chord
import Music.Frets ( noteAt, notesFromShape, standardTuning, recognizeChordFromShape )

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual =
  if expected == actual
    then putStrLn $ "PASS: " ++ label
    else putStrLn $ "FAIL: " ++ label ++
         "\n  expected: " ++ show expected ++
         "\n  got:      " ++ show actual ++ "\n"

main :: IO ()
main = do
  putStrLn "=== Testing Music.Note ==="

  check "noteIndex C" 0 (noteIndex C)
  check "noteIndex Fs" 6 (noteIndex Fs)
  check "noteIndex B" 11 (noteIndex B)

  check "noteFromIndex wraparound"
    C
    (noteFromIndex 12)

  check "noteFromIndex 25"
    Cs
    (noteFromIndex 25)

  check "interval C->E" 4 (interval C E)
  check "interval C->G" 7 (interval C G)
  check "interval B->C (wrap)" 1 (interval B C)
  check "interval Fs->D" 8 (interval Fs D)

  check "allNotes"
    [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    allNotes

  putStrLn "\n=== Testing Music.Chord ==="

  -- major
  check "C major"
    (Just (C, "maj"))
    (identifyChord [C, E, G])

  check "A major"
    (Just (A, "maj"))
    (identifyChord [A, Cs, E])

  -- minor
  check "A minor"
    (Just (A, "min"))
    (identifyChord [A, C, E])

  check "E minor"
    (Just (E, "min"))
    (identifyChord [E, G, B])

  -- diminished
  check "C diminished"
    (Just (C, "dim"))
    (identifyChord [C, Ds, Fs])

  -- augmented
  check "C augmented"
    (Just (C, "aug"))
    (identifyChord [C, E, Gs])

  -- dominant 7
  check "G7"
    (Just (G, "dom7"))
    (identifyChord [G, B, D, F])

  -- major 7
  check "Cmaj7"
    (Just (C, "maj7"))
    (identifyChord [C, E, G, B])

  -- minor 7
  check "Am7"
    (Just (A, "min7"))
    (identifyChord [A, C, E, G])

  -- inversions
  check "C major inversion [E,G,C]"
    (Just (C, "maj"))
    (identifyChord [E, G, C])

  check "A minor inversion"
    (Just (A, "min"))
    (identifyChord [C, E, A])

  -- invalid chord
  check "nonsense chord"
    Nothing
    (identifyChord [C, Cs, D])

  putStrLn "\n=== Testing Music.Frets ==="

  -- noteAt
  check "noteAt E 0"
    E
    (noteAt E 0)

  check "noteAt E 1"
    F
    (noteAt E 1)

  check "noteAt B 2"
    Cs
    (noteAt B 2)   -- B + 2 = C#

  -- notesFromShape simple cases
  check "notesFromShape: only string 6 fret 3 (G note)"
    [G]
    (notesFromShape standardTuning [Just 3, Nothing, Nothing, Nothing, Nothing, Nothing])

  check "notesFromShape: open D major (XX0232)"
    [D, A, D, Fs]
    (notesFromShape
       standardTuning
       [Nothing, Nothing, Just 0, Just 2, Just 3, Just 2])

  check "notesFromShape: C major (X32010)"
    [C, E, G, C, E]
    (notesFromShape
       standardTuning
       [Nothing, Just 3, Just 2, Just 0, Just 1, Just 0])

  -- recognizeChordFromShape
  check "recognizeChordFromShape: open E major (022100)"
    (Just (E, "maj"))
    (recognizeChordFromShape [Just 0, Just 2, Just 2, Just 1, Just 0, Just 0])

  check "recognizeChordFromShape: A minor (X02210)"
    (Just (A, "min"))
    (recognizeChordFromShape [Nothing, Just 0, Just 2, Just 2, Just 1, Just 0])

  check "recognizeChordFromShape: C major (X32010)"
    (Just (C, "maj"))
    (recognizeChordFromShape [Nothing, Just 3, Just 2, Just 0, Just 1, Just 0])
    
