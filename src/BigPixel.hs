-- |
-- Module      : Main
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : haskell2011


import Codec.BMP

import Data.ByteString                  (ByteString, pack)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Monad
import Data.Array
import Data.Word
import System.Environment
import System.Exit


-- Constants
-- ---------

-- Frames per second
--
fps :: Int
fps = 60

-- Minimum time (in seconds) between image writes.
--
writeInterval :: Float
writeInterval = 0.3

-- How many physical pixel per pixel art block?
--
pixelSize :: (Int, Int)
pixelSize = (8, 8)

-- Number of blocks on the initial canvas of an empty canvas (if not specified on the command line)?
--
initialCanvasSize :: (Int, Int)
initialCanvasSize = (16, 32)

-- Padding to the window border.
--
windowPadding :: Float
windowPadding = 40

-- The colour of the grid lines.
--
gridColor :: Color
gridColor = makeColor 0.9 0.9 0.9 1


-- Application state
-- -----------------

data State
  = State
    { fname          :: FilePath                    -- name of the image file
    , canvas         :: Array (Int, Int) Color      -- the image canvas
    , dirty          :: Bool                        -- 'True' iff there are unsaved changes
    , timeSinceWrite :: Float                       -- seconds passed since last write to image file
    }

initialState :: FilePath -> State
initialState name
  = State
    { fname          = name
    , canvas         = listArray ((0, 0), (maxX, maxY)) (repeat white)
    , dirty          = False
    , timeSinceWrite = 0
    }
  where
    maxX = fst initialCanvasSize - 1
    maxY = snd initialCanvasSize - 1


-- UI presentation
-- ---------------

-- Determine an appropriate window size for the given application state.
--
windowSize :: State -> Point
windowSize state 
  = (canvasX + 2 * windowPadding, canvasY + 2 * windowPadding)
  where
    (canvasX, canvasY) = canvasSize state

-- Size of the canvas in physical pixel.
--
canvasSize :: State -> Point
canvasSize state
  = (fromIntegral ((canvasWidth + 1) * fst pixelSize), fromIntegral ((canvasHeight + 1) * snd pixelSize))
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

-- Convert a canvas index to widget coordinates.
--
canvasToWidgetPos :: (Float, Float) -> (Int, Int) -> Point
canvasToWidgetPos (canvasWidth, canvasHeight) (i, j)
  = (x, y)
  where
    x = (fromIntegral i + 0.5) * width  - (canvasWidth  / 2)
    y = (fromIntegral j + 0.5) * height - (canvasHeight / 2)
    
    width                       = fromIntegral (fst pixelSize)
    height                      = fromIntegral (snd pixelSize)

-- Turn the application state into a picture (one frame).
--
drawCanvas :: State -> Picture
drawCanvas state
  = Pictures (map drawPixelBlock (assocs (canvas state)))
  where
    drawPixelBlock (pos, color) 
      = Translate x y $
          Pictures [ Color color (rectangleSolid width height)
                   , Color gridColor (rectangleWire width height)
                   ]
      where
        (x, y) = canvasToWidgetPos (canvasSize state) pos
        width  = fromIntegral (fst pixelSize)
        height = fromIntegral (snd pixelSize)

-- Produce the picture of the colour palette.
--
drawPalette :: Picture
drawPalette
  = Pictures (map drawPaletteBlock [(i, j) | i <- [0..15], j <- [0..15]])
  where
    drawPaletteBlock :: (Int, Int) -> Picture
    drawPaletteBlock pos@(i, j)
      = Translate x y $ Color col (rectangleSolid width height)
      where
        (x, y) = canvasToWidgetPos (16, 16) pos
        width  = fromIntegral (fst pixelSize)
        height = fromIntegral (snd pixelSize)
        col    = makeColor (red / 4) (green / 4) (blue / 4) ((5 - alpha) / 5)
        red    = fromIntegral $ ((i `div` 8) `mod` 2) * 2 + (j `div` 8) `mod` 2
        green  = fromIntegral $ ((i `div` 4) `mod` 2) * 2 + (j `div` 4) `mod` 2
        blue   = fromIntegral $ ((i `div` 2) `mod` 2) * 2 + (j `div` 2) `mod` 2
        alpha  = fromIntegral $ (i           `mod` 2) * 2 + j           `mod` 2

-- Draw a picture of the entire application window.
--
drawWindow :: State -> IO Picture
drawWindow = return . drawCanvas


-- Reading and writing of image files
-- ----------------------------------

-- Write the contents of the canvas to the image file.
--
writeImageFile :: State -> IO ()
writeImageFile state
  = writeBMP (fname state) (makeBMP (canvas state))

makeBMP :: Array (Int, Int) Color -> BMP
makeBMP canvas
  = packRGBA32ToBMP imageWidth imageHeight $
      pack (concat [ colorToWord8 (canvas!(x `div` fst pixelSize, y `div` snd pixelSize)) 
                   | y <- [0..imageHeight - 1], x <- [0..imageWidth - 1]])
  where
    (_, (maxX, maxY)) = bounds canvas

    canvasWidth       = maxX + 1
    canvasHeight      = maxY + 1
    
    imageWidth        = canvasWidth  * fst pixelSize
    imageHeight       = canvasHeight * snd pixelSize

-- Convert a Gloss colour to an RGBA value (quad of 'Word8's) for a BMP.
--
colorToWord8 :: Color -> [Word8]
colorToWord8 col
  = let (red, green, blue, alpha) = rgbaOfColor col
    in
    [toWord8 red, toWord8 green, toWord8 blue, toWord8 alpha]
  where
    toWord8 = truncate . (* 255)


-- Event handling
-- --------------

-- Process a single event.
--
handleEvent :: Event -> State -> IO State
handleEvent (EventKey (MouseButton LeftButton) Down mods mousePos) state
  = return $ draw (if shift mods == Up then black else white) mousePos state
handleEvent event state = return state

-- Draw onto the canvas
--
draw :: Color -> Point -> State -> State
draw col mousePos state
  = case windowPosToCanvas state mousePos of
      Nothing  -> state
      Just idx -> state { canvas = canvas state // [(idx, col)] 
                        , dirty  = True
                        }


-- Advance the application state
-- -----------------------------

-- Account for passing time.
--
stepState :: Float -> State -> IO State
stepState time state = maybeWriteFile $ state {timeSinceWrite = timeSinceWrite state + time}

-- Determine whether we should write the image file.
--
maybeWriteFile :: State -> IO State
maybeWriteFile state@(State {dirty = True, timeSinceWrite = time})
  | time > writeInterval 
  = do 
    { writeImageFile state
    ; return $ state {dirty = False, timeSinceWrite = 0}
    }
maybeWriteFile state
  = return state


-- The program body
-- ----------------

-- Kick of the event loop
--
main :: IO ()
main
  = do
      -- !!!FIXME: load file if it exists; otherwise, empty grid

    {   -- Read the image file name from the command like arguments
    ; args <- getArgs
    ; when (length args /= 1) $ 
        do { putStrLn "BigPixel needs exactly one argument, the image file name"
           ; exitFailure
           }
    ; let [fname] = args
    
        -- Initialise the application state
    ; let state             = initialState fname
          initialWindowSize = windowSize state
    
       -- Enter the event loop
    ; playIO (InWindow "BigPixel" (round (fst initialWindowSize), round (snd initialWindowSize))
        (100, 50)) white fps state
        drawWindow handleEvent stepState
    }
