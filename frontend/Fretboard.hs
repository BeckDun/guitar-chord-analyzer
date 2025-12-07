

  
module Fretboard (runFretboard) where

import Music.Frets (standardTuning, standardTuningPitched, noteAt, recognizeChordFromShape, pitchedNotesFromShape)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Music.Note
import Music.Frets
import Audio
import Control.Concurrent (forkIO)


-- GUI State
data GUIState = GUIState
  { selectedFrets :: [Maybe Int]   -- length 6, Nothing = muted
  , audioState    :: AudioState
  }

initialState :: AudioState -> GUIState
initialState audio = GUIState
  { selectedFrets = replicate 6 Nothing
  , audioState    = audio
  }


-- GUI Config
data FretboardConfig = FretboardConfig
  { numStrings     :: Int
  , numFrets       :: Int
  , fretWidth      :: Float
  , stringSpacing  :: Float
  , fretSpacing    :: Float
  , windowWidth    :: Int
  , windowHeight   :: Int
  }

defaultConfig :: FretboardConfig
defaultConfig = FretboardConfig
  { numStrings    = 6
  , numFrets      = 14
  , fretWidth     = 2.0
  , stringSpacing = 40.0
  , fretSpacing   = 60.0
  , windowWidth   = 1000
  , windowHeight  = 600
  }


-- GUI
runFretboard :: IO ()
runFretboard = do
  audio <- initAudio
  let cfg = defaultConfig
      window = InWindow "Interactive Guitar Fretboard"
                        (windowWidth cfg, windowHeight cfg)
                        (100,100)
  playIO
    window
    white
    30
    (initialState audio)
    (return . drawWorld cfg)
    (handleEventIO cfg)
    (\_ w -> return w)
  closeAudio audio


-- Draw world
drawWorld :: FretboardConfig -> GUIState -> Picture
drawWorld cfg st =
  Pictures
    [ drawFretboard cfg
    , drawSelectedNotes cfg st
    , drawChordLabel cfg st
    , drawMuteButtons cfg st
    , drawPlayButton cfg
    ]


-- Draw Fretboard
drawFretboard :: FretboardConfig -> Picture
drawFretboard cfg =
  Pictures $
    drawStrings cfg
    ++ drawFrets cfg
    ++ drawFretMarkers cfg
    ++ drawFretNumbers cfg


-- Horizontal strings
drawStrings :: FretboardConfig -> [Picture]
drawStrings cfg =
  let startX = -400
      endX   = startX + fromIntegral (numFrets cfg) * fretSpacing cfg

      topY = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
      ys = [ topY - fromIntegral i * stringSpacing cfg
           | i <- [0..numStrings cfg - 1]
           ]
  in map (\y -> Color (greyN 0.3) $ Line [(startX, y), (endX, y)]) ys


-- Vertical frets, including a thick nut
drawFrets :: FretboardConfig -> [Picture]
drawFrets cfg =
  let startX  = -400
      topY    =  fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
      bottomY = -topY
      xs =
        [ startX + fromIntegral f * fretSpacing cfg
        | f <- [0..numFrets cfg]
        ]

      nut  = Color black $ Polygon
             [ (head xs - 3, bottomY)
             , (head xs + 3, bottomY)
             , (head xs + 3, topY)
             , (head xs - 3, topY)
             ]

      otherFrets = map
        (\x -> Color (greyN 0.5) (Line [(x, bottomY), (x, topY)]))
        (tail xs)
  in nut : otherFrets


drawFretMarkers :: FretboardConfig -> [Picture]
drawFretMarkers cfg =
  let markerFrets = [3,5,7,9,12]
      startX = -400
      centerY = 0
  in
    [ let x = startX + fromIntegral f * fretSpacing cfg - fretSpacing cfg/2
      in if f == 12
           then Pictures
                [ Translate x (centerY + 20) $ Color (greyN 0.7) $ circleSolid 5
                , Translate x (centerY - 20) $ Color (greyN 0.7) $ circleSolid 5
                ]
           else Translate x centerY $ Color (greyN 0.7) $ circleSolid 5
    | f <- markerFrets
    ]


drawFretNumbers :: FretboardConfig -> [Picture]
drawFretNumbers cfg =
  let bottomY = -(fromIntegral (numStrings cfg - 1) * stringSpacing cfg) / 2
      startX = -400
  in
    [ let x = startX + fromIntegral f * fretSpacing cfg - fretSpacing cfg/2
      in Translate x (bottomY - 30)
          $ Scale 0.15 0.15
          $ Color black
          $ Text (show f)
    | f <- [1..numFrets cfg]
    ]


-- Draw mute buttons
drawMuteButtons :: FretboardConfig -> GUIState -> Picture
drawMuteButtons cfg st =
  let
      startX = -400
      buttonX = startX - 80
      topY = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
  in Pictures
       [ let y = topY - fromIntegral i * stringSpacing cfg
             isMuted = fret == Nothing
         in Translate buttonX y $ drawMuteButton isMuted
       | (i, fret) <- zip [0..] (selectedFrets st)
       ]


drawMuteButton :: Bool -> Picture
drawMuteButton isMuted =
  let col = if isMuted then red else black
      line1 = Polygon [ (-10, -8), (-8, -10), (10, 8), (8, 10) ]
      line2 = Polygon [ (-10, 8), (-8, 10), (10, -8), (8, -10) ]
  in Pictures
       [ Color col line1
       , Color col line2
       ]


-- Draw selected notes
drawSelectedNotes :: FretboardConfig -> GUIState -> Picture
drawSelectedNotes cfg st =
  let frets  = selectedFrets st
      startX = -400
      topY   = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
  in Pictures
       [ let x = startX 
                 + fromIntegral fret * fretSpacing cfg
                 - fretSpacing cfg / 2
             y = topY - fromIntegral stringIndex * stringSpacing cfg

             openN = reverse standardTuning !! stringIndex
             note  = noteAt openN fret
         in Translate x y $ drawBox note

       | (stringIndex, Just fret) <- zip [0..] frets
       ]


drawBox :: Show a => a -> Picture
drawBox note = Pictures
    [ Color red $ rectangleSolid 30 20
    , Translate (-10) (-5)
        $ Scale 0.1 0.1
        $ Color black
        $ Text (show note)
    ]


-- Draw chord label
drawChordLabel :: FretboardConfig -> GUIState -> Picture
drawChordLabel cfg st = 
  let 
      shape = reverse (selectedFrets st)
      
      chordText = case recognizeChordFromShape shape of
        Just (root, quality) -> show root ++ " " ++ quality
        Nothing              -> "Unknown chord"
      
      bottomY = -(fromIntegral (numStrings cfg - 1) * stringSpacing cfg) / 2
      boxY = bottomY - 70
  in 
      Translate 0 boxY $ Pictures
        [ Color (greyN 0.9) $ rectangleSolid 200 40
        , Translate (-80) (-8)
            $ Scale 0.2 0.2
            $ Color black
            $ Text chordText
        ]


-- Draw play button
drawPlayButton :: FretboardConfig -> Picture
drawPlayButton cfg =
  let bottomY = -(fromIntegral (numStrings cfg - 1) * stringSpacing cfg) / 2
      buttonY = bottomY - 120
  in Translate 0 buttonY $ Pictures
       [ Color (greyN 0.8) $ rectangleSolid 80 30
       , Translate (-25) (-8)
           $ Scale 0.15 0.15
           $ Color black
           $ Text "Play"
       ]


-- Event handling (IO version)
handleEventIO :: FretboardConfig -> Event -> GUIState -> IO GUIState
handleEventIO cfg (EventKey (MouseButton LeftButton) Down _ mousePos) st
  | clickedPlayButton cfg mousePos = do
      let shape = reverse (selectedFrets st)
          notes = pitchedNotesFromShape standardTuningPitched shape
      _ <- forkIO $ playChord (audioState st) 150 notes
      return st
  
  | otherwise = return $ handleEvent cfg mousePos st

handleEventIO _ _ st = return st


-- Pure event handler for fret/mute clicks
handleEvent :: FretboardConfig -> (Float, Float) -> GUIState -> GUIState
handleEvent cfg mousePos st =
  case clickedMuteButton cfg mousePos of
    Just stringIdx ->
      let old = selectedFrets st
          current = old !! stringIdx
          new = case current of
                  Nothing -> Just 0
                  Just _  -> Nothing
      in st { selectedFrets = replaceIndex stringIdx new old }
    
    Nothing ->
      case clickedFretAndString cfg mousePos of
        Just (stringIdx, fretIdx) ->
          let old = selectedFrets st
              new = replaceIndex stringIdx (Just fretIdx) old
          in st { selectedFrets = new }
        Nothing -> st


-- Check if play button was clicked
clickedPlayButton :: FretboardConfig -> (Float, Float) -> Bool
clickedPlayButton cfg (mx, my) =
  let bottomY = -(fromIntegral (numStrings cfg - 1) * stringSpacing cfg) / 2
      buttonY = bottomY - 120
  in abs mx < 40 && abs (my - buttonY) < 15


-- Check if mute button was clicked
clickedMuteButton :: FretboardConfig -> (Float, Float) -> Maybe Int
clickedMuteButton cfg (mx, my) =
  let
      startX = -400
      buttonX = startX - 80
      
      topY = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
      stringYs = [ topY - fromIntegral i * stringSpacing cfg
                 | i <- [0..numStrings cfg - 1]
                 ]
      
      nearButtonX = abs (mx - buttonX) < 15
      stringHit = lookupWithin 20 my stringYs
  in
      if nearButtonX then stringHit else Nothing


replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i val xs =
  take i xs ++ [val] ++ drop (i+1) xs


-- Click mapping for frets
clickedFretAndString :: FretboardConfig -> (Float, Float)
                     -> Maybe (Int, Int)
clickedFretAndString cfg (mx, my) =
  let
      topY = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
      stringYs =
        [ topY - fromIntegral i * stringSpacing cfg
        | i <- [0..numStrings cfg - 1]
        ]

      stringHit = lookupWithin 20 my stringYs

      startX = -400
      fretXs =
        [ startX + fromIntegral f * fretSpacing cfg
        | f <- [0..numFrets cfg]
        ]

      fretHit = findFret fretXs mx
  in
      (,) <$> stringHit <*> fretHit


lookupWithin :: Float -> Float -> [Float] -> Maybe Int
lookupWithin eps y ys =
  let dists = zip [0..] (map (\p -> abs (p - y)) ys)
  in case filter (\(_, d) -> d < eps) dists of
       ((i,_):_) -> Just i
       _         -> Nothing


findFret :: [Float] -> Float -> Maybe Int
findFret fretXs mx =
  let
      fretsPassed = length (filter (< mx) fretXs)
      maxFret = max 0 (length fretXs - 1)
      fret = min fretsPassed maxFret
  in Just fret