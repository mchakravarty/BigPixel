import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Data.Array


-- Constants
-- ---------

-- Frames per second
--
fps :: Int
fps = 60

-- How many physical pixel per pixel art block?
--
pixelSize :: (Int, Int)
pixelSize = (10, 10)

-- Number of blocks on the initial canvas of an empty canvas (if not specified on the command line)?
--
initialCanvasSize :: (Int, Int)
initialCanvasSize = (16, 16)

-- The colour of the grid lines.
--
gridColor :: Color
gridColor = makeColor 0 0 0 0.1


-- Application state
-- -----------------

data State
  = State
    { canvas :: Array (Int, Int) Color  -- !!!FIXME: allow texture in pixel blocks á la Minecraft
    }

initialState :: State
initialState
  = State
    { canvas = listArray ((0, 0), (maxX, maxY)) (repeat white)
    }
  where
    maxX = fst initialCanvasSize - 1
    maxY = snd initialCanvasSize - 1


-- UI presentation
-- ---------------

-- Determine an appropriate window size for the given application state.
--
windowSize :: State -> Point
windowSize = canvasSize

-- Size of the canvas in physical pixel.
--
canvasSize :: State -> Point
canvasSize state
  = (fromIntegral (canvasWidth * fst pixelSize), fromIntegral (canvasHeight * snd pixelSize))
  where
    (canvasWidth, canvasHeight) = snd (bounds (canvas state))

-- Convert window coordinates to a canvas index.
--
windowPosToCanvas :: State -> Point -> Maybe (Int, Int)
windowPosToCanvas state (x, y)
  |  x_shifted < 0 || x_shifted >= canvasWidth
  || y_shifted < 0 || y_shifted >= canvasHeight
  = Nothing
  | otherwise
  = Just (truncate x_shifted `div` fst pixelSize, truncate y_shifted `div` snd pixelSize)
  where 
    (canvasWidth, canvasHeight) = canvasSize state
    halfCanvasWidth             = canvasWidth  / 2
    halfCanvasHeight            = canvasHeight / 2
    
    x_shifted = x + halfCanvasWidth
    y_shifted = y + halfCanvasHeight

-- Turn the application state into a picture (one frame).
--
drawCanvas :: State -> Picture
drawCanvas state
  = Pictures (map drawPixelBlock (assocs (canvas state)))
  where
    drawPixelBlock ((xPos, yPos), color) 
      = Translate x y $
          Pictures [ Color color (rectangleSolid width height)
                   , Color gridColor (rectangleWire width height)
                   ]
      where
        x = (fromIntegral xPos + 0.5) * width  - (canvasWidth  / 2)
        y = (fromIntegral yPos + 0.5) * height - (canvasHeight / 2)
  
        width                       = fromIntegral (fst pixelSize)
        height                      = fromIntegral (snd pixelSize)
        (canvasWidth, canvasHeight) = canvasSize state


-- Event handling
-- --------------

-- Process a single event.
--
handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Down mods mousePos) state
  = draw (if shift mods == Up then black else white) mousePos state
handleEvent event state = state

-- Draw onto the canvas
--
draw :: Color -> Point -> State -> State
draw col mousePos state
  = case windowPosToCanvas state mousePos of
      Nothing  -> state
      Just idx -> state { canvas = canvas state // [(idx, col)]}


-- Advance the application state
-- -----------------------------

-- Account for passing time.
--
stepState :: Float -> State -> State
stepState time state = state


-- The program body
-- ----------------

-- Kick of the event loop
--
main :: IO ()
main
  = do
      -- !!!FIXME: get filename from args
      --   load file if it exists; otherwise, empty grid
    {   -- Initialise the application state
    ; let state             = initialState
          initialWindowSize = windowSize state
    
       -- Enter the event loop
    ; play (InWindow "BigPixel" (round (fst initialWindowSize), round (snd initialWindowSize))
        (100, 50)) white fps state
        drawCanvas handleEvent stepState
    }
