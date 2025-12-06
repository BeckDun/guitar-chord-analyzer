module Fretboard (runFretboard) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Music.Frets (Fret, ChordShape, recognizeChordFromShape)
import Music.Note  (Note)

-- | Configuration for the fretboard display
data FretboardConfig = FretboardConfig
  { numStrings   :: Int
  , numFrets     :: Int
  , fretWidth    :: Float
  , stringSpacing :: Float
  , fretSpacing  :: Float
  , windowWidth  :: Int
  , windowHeight :: Int
  }

-- | Default configuration for a 6-string, 14-fret guitar
defaultConfig :: FretboardConfig
defaultConfig = FretboardConfig
  { numStrings    = 6
  , numFrets      = 14
  , fretWidth     = 2.0
  , stringSpacing = 40.0
  , fretSpacing   = 60.0
  , windowWidth   = 1000
  , windowHeight  = 350
  }

------------------------------------------------------------
-- Existing drawing code (Max) – unchanged
------------------------------------------------------------

-- | Draw a single string (horizontal line)
drawString :: Float -> Float -> Float -> Picture
drawString y startX endX =
  Color (greyN 0.3) $ Line [(startX, y), (endX, y)]

-- | Draw a single fret (vertical line)
drawFret :: Float -> Float -> Float -> Picture
drawFret x startY endY =
  Color (greyN 0.5) $ Line [(x, startY), (x, endY)]

-- | Draw the nut (the first fret, thicker - vertical bar on the left)
drawNut :: Float -> Float -> Float -> Picture
drawNut x startY endY =
  Color black $ Polygon
    [ (x - 3, startY)
    , (x + 3, startY)
    , (x + 3, endY)
    , (x - 3, endY)
    ]

-- | Draw fret markers (dots at frets 3, 5, 7, 9, 12)
drawFretMarkers :: FretboardConfig -> [Picture]
drawFretMarkers config =
  let markerFrets = [3, 5, 7, 9, 12]
      startX = -400
      -- Place markers in the middle of the string range
      centerY = 0

      drawMarker fretNum =
        let x = startX + fromIntegral fretNum * fretSpacing config
                - fretSpacing config / 2
        in if fretNum == 12
           then Pictures
             [ Translate x (centerY + 20) $ Color (greyN 0.7) $ circleSolid 5
             , Translate x (centerY - 20) $ Color (greyN 0.7) $ circleSolid 5
             ]
           else Translate x centerY $ Color (greyN 0.7) $ circleSolid 5
  in map drawMarker markerFrets

-- | Draw all strings (horizontal lines)
drawStrings :: FretboardConfig -> [Picture]
drawStrings config =
  let startX  = -400
      endX    = startX + fromIntegral (numFrets config) * fretSpacing config
      -- Strings from top to bottom (high E to low E)
      topY    = fromIntegral (numStrings config - 1) * stringSpacing config / 2
      stringYs =
        [ topY - fromIntegral i * stringSpacing config
        | i <- [0 .. numStrings config - 1]
        ]
  in map (\y -> drawString y startX endX) stringYs

-- | Draw all frets (vertical lines)
drawFrets :: FretboardConfig -> [Picture]
drawFrets config =
  let topY    = fromIntegral (numStrings config - 1) * stringSpacing config / 2
      bottomY = -topY
      startX  = -400
      fretXs  =
        [ startX + fromIntegral i * fretSpacing config
        | i <- [0 .. numFrets config]
        ]

      -- Draw the nut (fret 0) thicker
      nut   = drawNut (head fretXs) bottomY topY
      frets = map (\x -> drawFret x bottomY topY) (tail fretXs)
  in nut : frets

-- | Draw fret numbers below the fretboard
drawFretNumbers :: FretboardConfig -> [Picture]
drawFretNumbers config =
  let bottomY = -(fromIntegral (numStrings config - 1) * stringSpacing config) / 2
      startX  = -400
      fretNums = [1 .. numFrets config]

      drawNumber num =
        let x = startX + fromIntegral num * fretSpacing config
                - fretSpacing config / 2
            y = bottomY - 30
        in Translate x y $ Scale 0.15 0.15 $ Color black $ Text (show num)
  in map drawNumber fretNums

-- | Combine all fretboard elements
drawFretboard :: FretboardConfig -> Picture
drawFretboard config = Pictures $
  drawStrings config
  ++ drawFrets config
  ++ drawFretMarkers config
  ++ drawFretNumbers config

------------------------------------------------------------
-- Aziz: State, event handling, chord display
------------------------------------------------------------

data AppState = AppState
  { appConfig   :: FretboardConfig
  , appShape    :: ChordShape        -- one Fret per string
  , appChordLbl :: String            -- e.g. "C maj7"
  }

initialState :: AppState
initialState =
  AppState
    { appConfig   = defaultConfig
    , appShape    = replicate (numStrings defaultConfig) Nothing
    , appChordLbl = "Click on frets to select a chord"
    }

-- Update one index in a list
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs =
  [ if idx == i then f x else x
  | (idx, x) <- zip [0 ..] xs
  ]

toggleFret :: Int -> Fret -> Fret
toggleFret fretIdx current =
  case current of
    Just n | n == fretIdx -> Nothing
    _                     -> Just fretIdx

updateChordLabel :: AppState -> AppState
updateChordLabel st =
  let shape = appShape st
      label =
        case recognizeChordFromShape shape of
          Just (root, name) -> show root ++ " " ++ name
          Nothing           -> "Unrecognized chord"
  in st { appChordLbl = label }

-- Convert mouse (x,y) into (stringIndex, fretNumber)
-- stringIndex: 0 .. numStrings-1 (0 = top string)
-- fretNumber:  0 = open, 1..numFrets = fretted
locateStringFret :: FretboardConfig -> (Float, Float) -> Maybe (Int, Int)
locateStringFret cfg (mx, my) =
  let nStr      = numStrings cfg
      nFrets    = numFrets cfg
      sSpacing  = stringSpacing cfg
      fSpacing  = fretSpacing cfg
      topY      = fromIntegral (nStr - 1) * sSpacing / 2
      bottomY   = -topY
      startX    = -400 :: Float
  in
    -- Check y within board area (with a small margin)
    if my < bottomY - sSpacing || my > topY + sSpacing
      then Nothing
      else
        let sIdxFloat = (topY - my) / sSpacing
            sIdx      = round sIdxFloat
        in if sIdx < 0 || sIdx >= nStr
             then Nothing
             else
               let relX = mx - startX
               in if relX < 0
                    -- left of nut → open string
                    then Just (sIdx, 0)
                    else
                      let fretIdx = floor (relX / fSpacing) + 1
                      in if fretIdx < 0 || fretIdx > nFrets
                           then Nothing
                           else Just (sIdx, fretIdx)

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) st =
  case locateStringFret (appConfig st) pos of
    Just (sIdx, fretIdx) ->
      let newShape = updateAt sIdx (toggleFret fretIdx) (appShape st)
      in pure $ updateChordLabel st { appShape = newShape }
    Nothing -> pure st
handleEvent _ st = pure st

drawChordText :: String -> Picture
drawChordText lbl =
  Translate 0 140 $
    Scale 0.2 0.2 $
      Color black (Text lbl)

drawApp :: AppState -> IO Picture
drawApp st =
  pure $
    Pictures
      [ drawFretboard (appConfig st)
      , drawChordText (appChordLbl st)
      ]

-- | Run the fretboard GUI with state + chord recognition
runFretboard :: IO ()
runFretboard =
  let cfg       = defaultConfig
      window    = InWindow "Guitar Chord Analyzer"
                           (windowWidth cfg, windowHeight cfg)
                           (100, 100)
      background = white
      fps        = 60
  in playIO
       window
       background
       fps
       initialState
       drawApp
       handleEvent
       (\_ st -> pure st)  -- no time-based updates

