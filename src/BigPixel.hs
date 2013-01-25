-- |
-- Module      : Main
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : haskell2011


import Codec.BMP

import Data.ByteString                  (ByteString, pack, unpack)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Exception as Exc
import Control.Monad
import Data.Array
import Data.Word
import System.Environment
import System.Exit
import System.IO.Error


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

-- Padding between window elements.
--
elementPadding :: Float
elementPadding = 50

-- The height of the colour indicator strip.
--
colourIndicatorHeight :: Float
colourIndicatorHeight = fromIntegral (fst pixelSize) * 2

-- The colour of the grid lines.
--
gridColor :: Color
gridColor = makeColor 0.9 0.9 0.9 1


-- Application state
-- -----------------

-- The colour grid that contains the drawing
--
type Canvas = Array (Int, Int) Color

-- Entire state of the applications
--
data State
  = State
    { fname          :: FilePath                    -- name of the image file
    , canvas         :: Canvas                      -- the image canvas
    , image          :: BMP                         -- BMP image version of the canvas
    , dirty          :: Bool                        -- 'True' iff there are unsaved changes
    , timeSinceWrite :: Float                       -- seconds passed since last write to image file
    , penDown        :: Maybe Color                 -- 'Just col' iff pen down with the given color
    , colour         :: Color                       -- colour of the pen
    }

initialState :: FilePath -> Canvas -> BMP -> State
initialState name initialCanvas initialImage
  = State
    { fname          = name
    , canvas         = initialCanvas
    , image          = initialImage
    , dirty          = False
    , timeSinceWrite = 0
    , penDown        = Nothing
    , colour         = white
    }


-- UI presentation
-- ---------------

-- Determine an appropriate window size for the given application state.
--
windowSize :: State -> Point
windowSize state 
  = (3 *  canvasX + 2 * windowPadding + 2 * elementPadding, canvasY + 2 * windowPadding)
  where
    (canvasX, canvasY) = canvasSize state

-- Size of the canvas in physical pixel.
--
canvasSize :: State -> Point
canvasSize state
  = (fromIntegral ((canvasWidth + 1) * fst pixelSize), 
     fromIntegral ((canvasHeight + 1) * snd pixelSize))
  where
    (canvasWidth, canvasHeight) = snd (bounds (canvas state))

-- Size of the palette in physical pixel.
--
paletteSize :: Point
paletteSize = (fromIntegral (16 * fst pixelSize), fromIntegral (16 * snd pixelSize))

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
    
    width  = fromIntegral (fst pixelSize)
    height = fromIntegral (snd pixelSize)

-- Turn the canvas in the application state into a picture (one frame).
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

-- Turn the image in the application state into a picture (one frame).
--
drawImage :: State -> Picture
drawImage = bitmapOfBMP . image

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
        (x, y) = canvasToWidgetPos paletteSize pos
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
drawWindow state 
  = return $ Pictures
             [ drawCanvas state
             -- , Translate (-imageOffset) 0 (drawImage state)
             , Translate paletteOffset 0               drawPalette
             , Translate paletteOffset (-colourOffset) colourIndicator
             ]
  where
    imageOffset   = elementPadding + fst (canvasSize state) / 2 +
                    fromIntegral (fst (bmpDimensions (image state))) / 2
    paletteOffset = elementPadding + fst (canvasSize state) / 2 + fst paletteSize / 2
    colourOffset  = 2 * colourIndicatorHeight + snd paletteSize / 2
    
    colourIndicator = Pictures
                      [ Color (colour state) (rectangleSolid (fst paletteSize) colourIndicatorHeight)
                      , Color gridColor      (rectangleWire  (fst paletteSize) colourIndicatorHeight)
                      ]


-- Reading and writing of image files
-- ----------------------------------

-- Try to read the image file and to convert it to a canvas. If that fails yield an empty canvas.
--
readImageFile :: FilePath -> IO (Canvas, BMP)
readImageFile fname
  = do 
    { result <- readBMP fname
    ; case result of
        Left err  -> do
                     { putStrLn ("BigPixel: error reading '" ++ fname ++ "': " ++ show err)
                     ; putStrLn "Delete the file if you want to replace it."
                     ; exitFailure
                     }
        Right bmp -> do
                     { let (bmpWidth, bmpHeight) = bmpDimensions bmp
                           canvasWidth           = bmpWidth  `div` fst pixelSize
                           canvasHeight          = bmpHeight `div` snd pixelSize
                     ; unless (bmpWidth  `mod` fst pixelSize == 0 &&
                               bmpHeight `mod` snd pixelSize == 0) $
                       do 
                       { putStrLn ("BigPixel: '" ++ fname ++ "' doesn't appear to be a BigPixel image")
                       ; putStrLn ("Expected the image size to be a multiple of " ++ 
                                   show (fst pixelSize) ++ "x" ++ show (snd pixelSize))
                       }
                     ; let stream  = unpack (unpackBMPToRGBA32 bmp)
                           indices = [(i, j) | j <- [0..bmpHeight - 1], i <- [0..bmpWidth - 1]]
                           image   = array ((0, 0), (bmpWidth - 1, bmpHeight - 1)) 
                                       (zip indices [word8ToColor quad | quad <- quads stream])
                           canvas0 = listArray ((0, 0), (canvasWidth - 1, canvasHeight - 1))
                                       [averageAt image (i, j) | i <- [0..canvasWidth  - 1]
                                                               , j <- [0..canvasHeight - 1]]
                     ; return (canvas0, bmp)
                     }
    }
    `Exc.catch` \exc ->
    if isDoesNotExistErrorType (ioeGetErrorType exc)
    then return (emptyCanvas, canvasToImage emptyCanvas)
    else do
    { putStrLn ("BigPixel: error reading '" ++ fname ++ "': " ++ show exc)
    ; putStrLn "Delete the file if you want to replace it."
    ; exitFailure
    }
  where
    maxX        = fst initialCanvasSize - 1
    maxY        = snd initialCanvasSize - 1
    emptyCanvas = listArray ((0, 0), (maxX, maxY)) (repeat white)
    
    quads :: [a] -> [[a]]
    quads []             = []
    quads (x:y:z:v:rest) = [x, y, z, v] : quads rest
    quads l              = [l]
                       
    averageAt image (i, j)
      = foldl1 addColors [ image ! (i * fst pixelSize + ioff, j * snd pixelSize + joff) 
                         | ioff <- [0..fst pixelSize - 1]
                         , joff <- [0..snd pixelSize - 1]]

-- Write the contents of the canvas to the image file.
--
writeImageFile :: State -> IO ()
writeImageFile state
  = writeBMP (fname state) (canvasToImage (canvas state))

-- Convert a canvas array into a BMP image file.
--
canvasToImage :: Array (Int, Int) Color -> BMP
canvasToImage canvas
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

-- Convert an RGBA value (quad of 'Word8's) for a BMP to a Gloss colour.
--
word8ToColor :: [Word8] -> Color
word8ToColor [red, green, blue, alpha]
  = makeColor (fromWord8 red) (fromWord8 green) (fromWord8 blue) (fromWord8 alpha)
  where
    fromWord8 = (/ 255) . fromIntegral
word8ToColor arg = error ("word8ToColor: not a quad: " ++ show arg)


-- Event handling
-- --------------

-- Process a single event.
--
handleEvent :: Event -> State -> IO State
handleEvent (EventKey (MouseButton LeftButton) Down mods mousePos) state
  = let newState = state { penDown = Just (if shift mods == Up then black else white) }
    in return $ draw mousePos newState
handleEvent (EventKey (MouseButton LeftButton) Up _mods _mousePos) state
  = return $ state {penDown = Nothing}
handleEvent (EventMotion mousePos) state
  = return $ draw mousePos state
handleEvent event state = return state

-- Draw onto the canvas.
--
-- NB: Does image conversion as well; i.e., only use once per frame in the current form.
--
draw :: Point -> State -> State
draw mousePos (state@State { penDown = Just col })
  = case windowPosToCanvas state mousePos of
      Nothing  -> state
      Just idx -> let newCanvas = canvas state // [(idx, col)]
                  in
                  state { canvas = newCanvas
                        -- , image  = canvasToImage newCanvas
                        , dirty  = True
                        }
draw _mousePos state
  = state


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
    {   -- Read the image file name from the command line arguments
    ; args <- getArgs
    ; when (length args /= 1) $ do
        { putStrLn "BigPixel: need exactly one argument, the image file name with suffix '.bmp'"
        ; exitFailure
        }
    ; let [fname] = args
    ; when (take 4 (reverse fname) /= reverse ".bmp") $ do
        { putStrLn "BigPixel: image file must have suffix '.bmp'"
        ; exitFailure
        }
    
        -- Read the image from the given file, or yield an empty canvas
    ; (canvas, image) <- readImageFile fname
    
        -- Initialise the application state
    ; let state             = initialState fname canvas image
          initialWindowSize = windowSize state
    
       -- Enter the event loop
    ; playIO (InWindow "BigPixel" (round (fst initialWindowSize), round (snd initialWindowSize))
        (100, 50)) white fps state
        drawWindow handleEvent stepState
    }
