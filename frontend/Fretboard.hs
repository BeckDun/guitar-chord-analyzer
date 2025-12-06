module Fretboard (runFretboard) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Music.Note
import Music.Frets


-- GUI State
-- Store selected frets for each of the 6 strings:
--   Nothing => not selected
--   Just n  => fret n is selected
data GUIState = GUIState
  { selectedFrets :: [Maybe Int]   -- length 6
  }

initialState :: GUIState
initialState = GUIState (replicate 6 Nothing)

drawMuteButtons :: FretboardConfig -> GUIState -> Picture
drawMuteButtons cfg st =
  let
      startX = -400
      buttonX = startX - 80  -- left of the nut
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
      thickness = 2
      -- Draw thick X using polygons
      line1 = Polygon [ (-10, -8), (-8, -10), (10, 8), (8, 10) ]
      line2 = Polygon [ (-10, 8), (-8, 10), (10, -8), (8, -10) ]
  in Pictures
       [ Color col line1
       , Color col line2
       ]

--GUI Config
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
  , windowHeight  = 350
  }



-- GUI
runFretboard :: IO ()
runFretboard =
  let cfg = defaultConfig
      window = InWindow "Interactive Guitar Fretboard"
                        (windowWidth cfg, windowHeight cfg)
                        (100,100)
  in play
        window
        white
        30
        initialState
        (drawWorld cfg)
        (handleEvent cfg)
        (\_ w -> w)      -- no automatic animation



-- Draw world
drawWorld :: FretboardConfig -> GUIState -> Picture
drawWorld cfg st =
  Pictures
    [ drawFretboard cfg
    , drawSelectedNotes cfg st
    , drawChordLabel cfg st
    , drawMuteButtons cfg st
    ]

drawChordLabel :: FretboardConfig -> GUIState -> Picture
drawChordLabel cfg st = 
  let 
        shape = reverse (selectedFrets st)
      
        chordText = case recognizeChordFromShape shape of
         Just (root, quality) -> show root ++ " " ++ quality
         Nothing              -> "Unknown chord"
      
      -- Position below the fretboard
        bottomY = -(fromIntegral (numStrings cfg - 1) * stringSpacing cfg) / 2
        boxY = bottomY - 70
  in 
      Translate 0 boxY $ Pictures
        [ Color (greyN 0.9) $ rectangleSolid 200 40   -- background
        , Translate (-80) (-8)
            $ Scale 0.2 0.2
            $ Color black
            $ Text chordText
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


--Draw selected notes--

drawSelectedNotes :: FretboardConfig -> GUIState -> Picture
drawSelectedNotes cfg st =
  let frets  = selectedFrets st
      startX = -400
      topY   = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
  in Pictures
       [ let x = startX 
                 + fromIntegral fret * fretSpacing cfg
                 - fretSpacing cfg / 2      -- shift into middle of the fret
             y = topY - fromIntegral stringIndex * stringSpacing cfg

             openN = reverse standardTuning !! stringIndex
             note  = noteAt openN fret
         in Translate x y $ drawBox note

       | (stringIndex, Just fret) <- zip [0..] frets
       ]


drawBox :: Show a => a -> Picture
drawBox note = Pictures
    [ Color red $ rectangleSolid 30 20          -- background box
    , Translate (-10) (-5)                         -- center the text
        $ Scale 0.1 0.1
        $ Color black
        $ Text (show note)
    ]


--Click detection--

handleEvent :: FretboardConfig -> Event -> GUIState -> GUIState
handleEvent cfg (EventKey (MouseButton LeftButton) Down _ mousePos) st =
  case clickedMuteButton cfg mousePos of
    Just stringIdx ->
      let old = selectedFrets st
          current = old !! stringIdx
          new = case current of
                  Nothing -> Just 0    -- unmute to open string
                  Just _  -> Nothing   -- mute the string
      in st { selectedFrets = replaceIndex stringIdx new old }
    
    Nothing ->
      case clickedFretAndString cfg mousePos of
        Just (stringIdx, fretIdx) ->
          let old = selectedFrets st
              new = replaceIndex stringIdx (Just fretIdx) old
          in st { selectedFrets = new }
        Nothing -> st

handleEvent _ _ st = st



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



--Click Mapping--

clickedFretAndString :: FretboardConfig -> (Float, Float)
                     -> Maybe (Int, Int)
clickedFretAndString cfg (mx, my) =
  let
    -- string Y positions
    topY = fromIntegral (numStrings cfg - 1) * stringSpacing cfg / 2
    stringYs =
      [ topY - fromIntegral i * stringSpacing cfg
      | i <- [0..numStrings cfg - 1]
      ]

    stringHit = lookupWithin 20 my stringYs

    -- fret X positions
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


-- fretXs is the list of fret-line x coordinates: [x0, x1, x2, ...]
-- mx is the mouse x coordinate
findFret :: [Float] -> Float -> Maybe Int
findFret fretXs mx =
  let
    -- how many fret lines are strictly to the left of the click
    fretsPassed = length (filter (< mx) fretXs)

    -- maximum playable fret index we'll return
    -- if fretXs has N lines, the maximum fret number we'll return is N-1
    -- (this keeps it safe even if the click is far to the right)
    maxFret = max 0 (length fretXs - 1)

    -- clamp into [0 .. maxFret]
    fret = min fretsPassed maxFret
  in Just fret

  