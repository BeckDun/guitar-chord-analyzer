module Music.Sound
  ( NoteSampleKey
  , SoundBank
  , samplePath
  , allSamplePaths
  , loadSoundBank
  ) where

import qualified Data.Map as M
import Sound.WAVE

-- | (string, fret) – 1-based string index, 0–14 fret
type NoteSampleKey = (Int, Int)

type SoundBank = M.Map NoteSampleKey WAVE

-- | Where each sample lives on disk
samplePath :: NoteSampleKey -> FilePath
samplePath (string, fret) =
  "audio/string" ++ show string ++ "_fret" ++ show fret ++ ".wav"

-- | All (key, path) pairs for a 6-string, 14-fret guitar
allSamplePaths :: [(NoteSampleKey, FilePath)]
allSamplePaths =
  [ ((s, f), samplePath (s, f))
  | s <- [1 .. 6]
  , f <- [0 .. 14]
  ]

-- | Load all samples into memory.
-- Max can later use this in his playback logic.
loadSoundBank :: IO SoundBank
loadSoundBank = do
  pairs <- mapM loadOne allSamplePaths
  pure (M.fromList pairs)
  where
    loadOne (key, path) = do
      wav <- getWAVEFile path
      pure (key, wav)

