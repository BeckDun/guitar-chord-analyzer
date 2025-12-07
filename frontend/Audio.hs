module Audio
  ( AudioState
  , initAudio
  , closeAudio
  , playPitchedNote
  , playChord
  ) where

import qualified SDL
import qualified SDL.Mixer as Mix
import Music.Note
import Control.Monad (forM_, forM)
import Control.Concurrent (threadDelay)
import Data.Map (Map)
import qualified Data.Map as Map

data AudioState = AudioState
  { noteSamples :: Map Int Mix.Chunk  -- keyed by absolute semitone index
  }

-- All pitched notes needed for guitar (E2 to F#5)
allGuitarNotes :: [PitchedNote]
allGuitarNotes = map pitchedNoteFromIndex [28..65]  -- E2=28, F#5=65

initAudio :: IO AudioState
initAudio = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 256
  
  samples <- forM allGuitarNotes $ \pnote -> do
    chunk <- loadNoteSample pnote
    return (pitchedNoteToIndex pnote, chunk)
  return $ AudioState (Map.fromList samples)

loadNoteSample :: PitchedNote -> IO Mix.Chunk
loadNoteSample (PitchedNote note octave) = do
  let filename = "audio/" ++ show note ++ show octave ++ ".wav"
  Mix.load filename

closeAudio :: AudioState -> IO ()
closeAudio _ = do
  Mix.closeAudio
  Mix.quit
  SDL.quit

playPitchedNote :: AudioState -> PitchedNote -> IO ()
playPitchedNote audioSt pnote =
  case Map.lookup (pitchedNoteToIndex pnote) (noteSamples audioSt) of
    Just chunk -> do
      _ <- Mix.play chunk
      return ()
    Nothing -> return ()

-- Play pitched notes in sequence with delay (in milliseconds)
playChord :: AudioState -> Int -> [PitchedNote] -> IO ()
playChord audioSt delayMs notes =
  forM_ notes $ \pnote -> do
    playPitchedNote audioSt pnote
    threadDelay (delayMs * 1000)