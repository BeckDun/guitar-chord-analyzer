module Audio
  ( AudioState
  , initAudio
  , closeAudio
  , playNote
  , playChord
  ) where

import qualified SDL
import qualified SDL.Mixer as Mix
import Music.Note
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)
import Data.Map (Map)
import qualified Data.Map as Map

data AudioState = AudioState
  { noteSamples :: Map Note Mix.Chunk
  }

initAudio :: IO AudioState
initAudio = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 256
  
  -- Load a sample for each note
  samples <- mapM loadNoteSample allNotes
  return $ AudioState (Map.fromList $ zip allNotes samples)

loadNoteSample :: Note -> IO Mix.Chunk
loadNoteSample note = do
  let filename = "audio/" ++ show note ++ ".wav"
  Mix.load filename

closeAudio :: AudioState -> IO ()
closeAudio _ = do
  Mix.closeAudio
  Mix.quit
  SDL.quit

playNote :: AudioState -> Note -> IO ()
playNote audioSt note =
  case Map.lookup note (noteSamples audioSt) of
    Just chunk -> Mix.play chunk >> return ()
    Nothing    -> return ()

-- Play notes in sequence with delay (in milliseconds)
playChord :: AudioState -> Int -> [Note] -> IO ()
playChord audioSt delayMs notes =
  forM_ notes $ \note -> do
    playNote audioSt note
    threadDelay (delayMs * 1000)  -- threadDelay uses microseconds