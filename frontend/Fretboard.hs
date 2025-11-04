module Fretboard (runFretboard) where

import Graphics.Gloss

-- | Configuration for the fretboard display
data FretboardConfig = FretboardConfig
  { numStrings :: Int
  , numFrets   :: Int
  , fretWidth  :: Float
  , stringSpacing :: Float
  , fretSpacing :: Float
  , windowWidth :: Int
  , windowHeight :: Int
  }

-- | Default configuration for a 6-string, 14-fret guitar
defaultConfig :: FretboardConfig
defaultConfig = FretboardConfig
  { numStrings = 6
  , numFrets = 14
  , fretWidth = 2.0
  , stringSpacing = 40.0
  , fretSpacing = 60.0
  , windowWidth = 1000
  , windowHeight = 350
  }

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
  Color black $ Polygon [
    (x - 3, startY),
    (x + 3, startY),
    (x + 3, endY),
    (x - 3, endY)
  ]

-- | Draw fret markers (dots at frets 3, 5, 7, 9, 12)
drawFretMarkers :: FretboardConfig -> [Picture]
drawFretMarkers config =
  let markerFrets = [3, 5, 7, 9, 12]
      startX = -400
      -- Place markers in the middle of the string range
      centerY = 0

      drawMarker fretNum =
        let x = startX + fromIntegral fretNum * fretSpacing config - fretSpacing config / 2
        in if fretNum == 12
           then Pictures [
             Translate x (centerY + 20) $ Color (greyN 0.7) $ circleSolid 5,
             Translate x (centerY - 20) $ Color (greyN 0.7) $ circleSolid 5
           ]
           else Translate x centerY $ Color (greyN 0.7) $ circleSolid 5
  in map drawMarker markerFrets

-- | Draw all strings (horizontal lines)
drawStrings :: FretboardConfig -> [Picture]
drawStrings config =
  let startX = -400
      endX = startX + fromIntegral (numFrets config) * fretSpacing config
      -- Strings from top to bottom (high E to low E)
      topY = fromIntegral (numStrings config - 1) * stringSpacing config / 2
      stringYs = [topY - fromIntegral i * stringSpacing config | i <- [0..numStrings config - 1]]
  in map (\y -> drawString y startX endX) stringYs

-- | Draw all frets (vertical lines)
drawFrets :: FretboardConfig -> [Picture]
drawFrets config =
  let topY = fromIntegral (numStrings config - 1) * stringSpacing config / 2
      bottomY = -topY
      startX = -400
      fretXs = [startX + fromIntegral i * fretSpacing config | i <- [0..numFrets config]]

      -- Draw the nut (fret 0) thicker
      nut = drawNut (head fretXs) bottomY topY
      -- Draw the rest of the frets normally
      frets = map (\x -> drawFret x bottomY topY) (tail fretXs)
  in nut : frets

-- | Draw fret numbers below the fretboard
drawFretNumbers :: FretboardConfig -> [Picture]
drawFretNumbers config =
  let bottomY = -(fromIntegral (numStrings config - 1) * stringSpacing config) / 2
      startX = -400
      fretNums = [1..numFrets config]
      drawNumber num =
        let x = startX + fromIntegral num * fretSpacing config - fretSpacing config / 2
            y = bottomY - 30
        in Translate x y $ Scale 0.15 0.15 $ Color black $ Text (show num)
  in map drawNumber fretNums

-- | Combine all fretboard elements
drawFretboard :: FretboardConfig -> Picture
drawFretboard config = Pictures $
  drawStrings config ++
  drawFrets config ++
  drawFretMarkers config ++
  drawFretNumbers config

-- | Run the fretboard GUI with a fixed, non-resizable window
runFretboard :: IO ()
runFretboard =
  let config = defaultConfig
      -- InWindow creates a fixed-size, non-resizable window
      window = InWindow "Guitar Fretboard - 6 Strings, 14 Frets"
                         (windowWidth config, windowHeight config)
                         (100, 100)
      background = white
  in display window background (drawFretboard config)
