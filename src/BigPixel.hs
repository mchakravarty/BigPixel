-- |
-- Module      : Main
-- Copyright   : [2013] Manuel M T Chakravarty & Leon A Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Portability : haskell2011
--
-- /Usage/
--
-- Provide the filename as the single command line argument.
--
-- Left mouse button          — draw with current colour
-- Left mouse button + Shift  — erase with transparency
-- Right mouse button         — erase with transparency
-- 'W', 'S', 'A', 'D'         - enlarge canvas to the top, bottom, left, and right, respectively
-- 'W', 'S', 'A', 'D' + Shift - shrink canvas from the top, bottom, left, and right, respectively
--
-- Canvas changes are automatically saved.

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
gridColor = makeColor 0.8 0.8 0.8 1

-- Fully transparent colour.
--
transparent :: Color
transparent = makeColor 0 0 0 0

-- Nearly (25% opaque) transparent colour.
--
nearlytransparent :: Color
nearlytransparent = makeColor 0.25 0.25 0.25 0.25


-- Application state
-- -----------------

-- The colour grid that contains the drawing
--
type Canvas = Array (Int, Int) Color

-- The subarea of the canvas currently in use.
--
type Area = ((Int, Int), (Int, Int))

-- Entire state of the applications
--
data State
  = State
    { fname          :: FilePath                    -- name of the image file
    , canvas         :: Canvas                      -- the image canvas
    , area           :: Area                        -- range of indices of the canvas used
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
    , area           = bounds initialCanvas
    , image          = initialImage
    , dirty          = False
    , timeSinceWrite = 0
    , penDown        = Nothing
    , colour         = black
    }

-- Yield the width and height of an area.
--
areaSize :: Area -> (Int, Int)
areaSize ((minX, minY), (maxX, maxY))
  = (maxX - minX + 1, maxY - minY + 1)    

-- Check whether one area is completely contained in another.
--
containedWithin :: Area -> Area -> Bool
((minX1, minY1), (maxX1, maxY1)) `containedWithin` ((minX2, minY2), (maxX2, maxY2))
  = minX1 >= minX2 && minY1 >= minY2 && maxX1 <= maxX2 && maxY1 <= maxY2

-- Compute the smallest area containing the two given ones.
--
unionArea :: Area -> Area -> Area
((minX1, minY1), (maxX1, maxY1)) `unionArea` ((minX2, minY2), (maxX2, maxY2))
  = ((minX1 `min` minX2, minY1 `min` minY2), (maxX1 `max` maxX2, maxY1 `max` maxY2))

-- Vector addition
--
vplus :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i, j) `vplus` (k, l) = (i + k, j + l)    

-- Vector subtraction
--
vminus :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i, j) `vminus` (k, l) = (i - k, j - l)    


-- UI presentation
-- ---------------

-- Determine an appropriate window size for the given application state.
--
windowSize :: State -> Point
windowSize state 
  = (2 * (canvasW + paletteW) + 2 * windowPadding + 2 * elementPadding, 
     (20 + 1.5 * canvasH + 2 * windowPadding) `max` (1.5 * paletteH + 2 * windowPadding))
  where
    (canvasW,  canvasH)  = canvasSize state
    (paletteW, paletteH) = paletteSize

-- Size of the canvas in physical pixel.
--
canvasSize :: State -> Point
canvasSize state
  = (fromIntegral (width  * fst pixelSize),
     fromIntegral (height * snd pixelSize))
  where
    (width, height) = areaSize (area state)

-- Size of the palette in physical pixel.
--
paletteSize :: Point
paletteSize = (fromIntegral (16 * fst pixelSize), fromIntegral (16 * snd pixelSize))

-- Convert window coordinates to a canvas index.
--
windowPosToCanvas :: (Float, Float) -> Point -> Maybe (Int, Int)
windowPosToCanvas (canvasWidth, canvasHeight) (x, y)
  |  x_shifted < 0 || x_shifted >= canvasWidth
  || y_shifted < 0 || y_shifted >= canvasHeight
  = Nothing
  | otherwise
  = Just (truncate x_shifted `div` fst pixelSize, truncate y_shifted `div` snd pixelSize)
  where 
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
  = let (start, end) = area state
    in
    Pictures $
      map drawPixelBlock [(pos `vminus` start, canvas state!pos) | pos <- range (start, end)]
  where
    drawPixelBlock (pos, color) 
      = Translate x y $
          Pictures [ rectangleChecker width height
                   , Color color (rectangleSolid width height)
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
    drawPaletteBlock pos
      = Translate x y $ Pictures [ rectangleChecker width height
                                 , Color (paletteColour pos) (rectangleSolid width height)
                                 ]
      where
        (x, y) = canvasToWidgetPos paletteSize pos
        width  = fromIntegral (fst pixelSize)
        height = fromIntegral (snd pixelSize)

-- Draw a checker rectangle with a wire frame.
--
-- Width and height must be divisible by 2.
--
rectangleChecker :: Float -> Float -> Picture
rectangleChecker width height
  = Pictures [ Translate (-w4) (-h4) $ Color (greyN 0.90) (rectangleSolid w2 h2)
             , Translate (-w4) ( h4) $ Color white        (rectangleSolid w2 h2)
             , Translate ( w4) (-h4) $ Color white        (rectangleSolid w2 h2)
             , Translate ( w4) ( h4) $ Color (greyN 0.90) (rectangleSolid w2 h2)
             , Translate 0     0     $ Color (greyN 0.95) (Line [(0, -h2), (0, h2)])
             , Translate 0     0     $ Color (greyN 0.95) (Line [(-h2, 0), (h2, 0)])
             , Translate 0     0     $ Color gridColor (rectangleWire width height)
             ]
  where
    w2 = width  / 2
    h2 = height / 2
    w4 = width  / 4
    h4 = height / 4

-- Compute the colour of the palette at a particular index position.
--
-- 8-bit palette: RRGIBBGT
--
-- * RR = 2-bit red
-- * GG = 2-bit green
-- * BB = 2-bit blue
-- * I  = 1-bit brightness
-- * T  = 1-bit transparency
--
-- Intensity scales
--
-- * 00 = 0% (irrespective of brightness)
-- * 01 = brightness ? 60%  : 30%
-- * 10 = brightness ? 80%  : 40%
-- * 11 = brightness ? 100% : 50%
--
-- Transparency is 50%.
--
-- Special values
--
-- * 00010000 = 25% transparent (black)
-- * 00010001 = fully transparent (black)
--
-- Here, i = 4 MSBs and j = 4 LSBs.
--
paletteColour :: (Int, Int) -> Color
paletteColour (i, j) = paletteColour' (i, 15 - j)
  where
    paletteColour' (1, 0) = nearlytransparent
    paletteColour' (1, 1) = transparent
    paletteColour' (i, j)
      = makeColor (scale red / 100) (scale green / 100) (scale blue / 100) (if transparency == 1 then 0.5 else 1)
      where
        red          = fromIntegral $ ((i `div` 8) `mod` 2) * 2 + (i `div` 4) `mod` 2
        green        = fromIntegral $ ((i `div` 2) `mod` 2) * 2 + (j `div` 2) `mod` 2
        blue         = fromIntegral $ ((j `div` 8) `mod` 2) * 2 + (j `div` 4) `mod` 2
        brightness   = fromIntegral $ (i           `mod` 2)
        transparency = fromIntegral $ (j           `mod` 2)
    
        scale 0 = 0
        scale x = (40 + (60 / 3) * x) * (if brightness == 0 then 0.5 else 1)

{-
-- Compute the colour of the palette at a particular index position, but use the transparency of the given colour.
--
paletteColourWithTransparencyOf :: Color -> (Int, Int) -> Color
paletteColourWithTransparencyOf col idx
  = makeColor r g b a
  where
    (r, g, b, _) = rgbaOfColor $ paletteColour idx
    (_, _, _, a) = rgbaOfColor col
    -}

{-
-- Adjust a colour transparency (alpha value) for the given index position in the transparency palette.
--
transparencyColour :: Color -> Int -> Color
transparencyColour col i
  = makeColor r g b (fromIntegral i / 16)
  where
    (r, g, b, _a) = rgbaOfColor col
-}

-- Draw a picture of the entire application window.
--
drawWindow :: State -> IO Picture
drawWindow state 
  = return $ Pictures
             [ drawCanvas state
             , Translate (-40)         sizeOffset      canvasSizeText
                        -- ^^FIXME: Gloss doesn't seem to center text :(
             -- , Translate (-imageOffset) 0 (drawImage state)
             , Translate paletteOffset 0               drawPalette
             , Translate paletteOffset (-colourOffset) colourIndicator
             -- , Translate paletteOffset colourOffset    (drawTransparency (colour state))
             ]
  where
    imageOffset   = elementPadding + fst (canvasSize state) / 2 +
                    fromIntegral (fst (bmpDimensions (image state))) / 2
    paletteOffset = elementPadding + fst (canvasSize state) / 2 + fst paletteSize / 2
    colourOffset  = 2 * colourIndicatorHeight + snd paletteSize / 2
    sizeOffset    = snd (canvasSize state) / 2 + 20
    
    colourIndicator = Pictures $
                      [ Translate (fromIntegral i * pixelWidth  + pixelWidth  / 2) 
                                  (fromIntegral j * pixelHeight + pixelHeight / 2) $
                          rectangleChecker pixelWidth pixelHeight
                      | i <- [-8..7], j <- [-1..0] ] ++
                      [ Color (colour state) (rectangleSolid (fst paletteSize) colourIndicatorHeight)
                      , Color gridColor      (rectangleWire  (fst paletteSize) colourIndicatorHeight)
                      ]
    pixelWidth      = fromIntegral $ fst pixelSize
    pixelHeight     = fromIntegral $ snd pixelSize
                      
    canvasSizeText = let (width, height) = areaSize (area state)
                     in
                     Scale 0.2 0.2 (Text (show width ++ "x" ++ show height))


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
    then return (emptyCanvas, canvasToImage emptyCanvas (bounds emptyCanvas))
    else do
    { putStrLn ("BigPixel: error reading '" ++ fname ++ "': " ++ show exc)
    ; putStrLn "Delete the file if you want to replace it."
    ; exitFailure
    }
  where
    maxX        = fst initialCanvasSize - 1
    maxY        = snd initialCanvasSize - 1
    emptyCanvas = listArray ((0, 0), (maxX, maxY)) (repeat transparent)
    
    quads :: [a] -> [[a]]
    quads []             = []
    quads (x:y:z:v:rest) = [x, y, z, v] : quads rest
    quads l              = [l]
                       
    averageAt image (i, j)
      -- = clipColour $ image ! (i * fst pixelSize + fst pixelSize `div` 2, 
      --                         j * snd pixelSize + snd pixelSize `div` 2)
      = clipColour $
          foldl1 (mixColors 0.5 0.5) [ image ! (i * fst pixelSize + ioff, j * snd pixelSize + joff) 
                                     | ioff <- [0..fst pixelSize - 1]
                                     , joff <- [0..snd pixelSize - 1]]

-- Write the contents of the canvas to the image file.
--
writeImageFile :: State -> IO ()
writeImageFile state
  = writeBMP (fname state) (canvasToImage (canvas state) (area state))

-- Convert the specified area of a canvas array into a BMP image file.
--
canvasToImage :: Array (Int, Int) Color -> Area -> BMP
canvasToImage canvas area
  = packRGBA32ToBMP imageWidth imageHeight $
      pack (concat [ colorToWord8 (canvas!(minX + x `div` fst pixelSize, 
                                           minY + y `div` snd pixelSize)) 
                   | y <- [0..imageHeight - 1], x <- [0..imageWidth - 1]])
  where
    (canvasWidth, canvasHeight) = areaSize area
    ((minX, minY), _)           = area
    
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

-- Clip a colour to the BigPixel 8-bit colour space
--
clipColour :: Color -> Color
clipColour col
  | transparent = makeColor 0 0 0 0
  | black       = makeColor 0 0 0 1
  | otherwise   = makeColor red' green' blue' alpha'
  where
    (red, green, blue, alpha) = rgbaOfColor col
    
    black                                  = averageBrightness [red, green, blue] < 0.15
    transparent                            = alpha < 0.25
    alpha' | alpha >= 0.25 && alpha < 0.75 = 0.5
           | otherwise                     = 1
    
    bright = averageBrightness [red, green, blue] >= 0.55
    red'   = clip red
    green' = clip green
    blue'  = clip blue
    
    averageBrightness cols = sum significantCols / fromIntegral (length significantCols)
      where
        significantCols = [col | col <- cols, col >= 0.15]
    
    clip c | c < 0.15           = 0
           | bright && c < 0.70 = 0.6
           | bright && c < 0.90 = 0.8
           | bright             = 1
           | c > 0.45           = 0.5
           | c > 0.35           = 0.4
           | otherwise          = 0.3


-- Event handling
-- --------------

-- Process a single event.
--
handleEvent :: Event -> State -> IO State

  -- Drawing and colour selection
handleEvent (EventKey (MouseButton LeftButton) Down mods mousePos) state
  = let newState = state { penDown = Just (if shift mods == Up then colour state else transparent) }
    in return $ draw mousePos newState
handleEvent (EventKey (MouseButton RightButton) Down mods mousePos) state
  = let newState = state { penDown = Just transparent }
    in return $ draw mousePos newState
handleEvent (EventKey (MouseButton LeftButton) Up _mods mousePos) state
  = return $ selectColour mousePos (state {penDown = Nothing})
handleEvent (EventKey (MouseButton RightButton) Up _mods mousePos) state
  = return $ state {penDown = Nothing}
handleEvent (EventMotion mousePos) state
  = return $ draw mousePos state

  -- Alter canvas size
handleEvent (EventKey (Char 'w') Down mods _mousePos) state
  = return $ resize ((0, 0), (0, 1)) state
handleEvent (EventKey (Char 'W') Down mods _mousePos) state
  = return $ resize ((0, 0), (0, -1)) state
handleEvent (EventKey (Char 's') Down mods _mousePos) state
  = return $ resize ((0, -1), (0, 0)) state
handleEvent (EventKey (Char 'S') Down mods _mousePos) state
  = return $ resize ((0, 1), (0, 0)) state
handleEvent (EventKey (Char 'a') Down mods _mousePos) state
  = return $ resize ((-1, 0), (0, 0)) state
handleEvent (EventKey (Char 'A') Down mods _mousePos) state
  = return $ resize ((1, 0), (0, 0)) state
handleEvent (EventKey (Char 'd') Down mods _mousePos) state
  = return $ resize ((0, 0), (1, 0)) state
handleEvent (EventKey (Char 'D') Down mods _mousePos) state
  = return $ resize ((0, 0), (-1, 0)) state

  -- Unhandled event
handleEvent event state = return state

-- Draw onto the canvas if mouse position is within canvas boundaries.
--
-- NB: Does image conversion as well; i.e., only use once per frame in the current form.
--
draw :: Point -> State -> State
draw mousePos (state@State { penDown = Just col })
  = case windowPosToCanvas (canvasSize state) mousePos of
      Nothing  -> state
      Just idx -> let base      = fst (area state)
                      newCanvas = canvas state // [(base `vplus` idx, col)]
                  in
                  state { canvas = newCanvas
                        -- , image  = canvasToImage newCanvas ??
                        , dirty  = True
                        }
draw _mousePos state
  = state

-- Select a colour if mouse position is within palette boundaries or a transparency if mouse position is within
-- transparency picker boundaries.
--
selectColour :: Point -> State -> State
selectColour mousePos state
  = case windowPosToCanvas paletteSize paletteAdjustedMousePos of
      -- Nothing  -> case windowPosToCanvas pickerSize pickerAdjustedMousePos of
      --               Nothing     -> state
      --               Just (i, _) -> state { colour = transparencyColour (colour state) i }
      -- Just idx -> state { colour = paletteColourWithTransparencyOf (colour state) idx }
      Nothing  -> state
      Just idx -> state { colour = paletteColour idx }
  where
    paletteOffsetX          = elementPadding + fst (canvasSize state) / 2 + fst paletteSize / 2
    -- pickerOffsetY           = 2 * colourIndicatorHeight + snd paletteSize / 2
    paletteAdjustedMousePos = (fst mousePos - paletteOffsetX, snd mousePos)
    -- pickerAdjustedMousePos  = (fst mousePos - paletteOffsetX, snd mousePos - pickerOffsetY)

-- Resize the used canvas area.
--
-- We only change the actual canvas array if it needs to grow beyond its current size. If it,
-- shrinks, we leave it as it is to enable undoing the shrinking by growing it again.
--
resize :: Area -> State -> State
resize ((dminX, dminY), (dmaxX, dmaxY)) state
  | newWidth >= 2 && newHeight >= 2
  = state { canvas = newCanvas, area = newArea, dirty  = True }
  | otherwise
  = state
  where
    ((minX, minY), (maxX, maxY)) = area state
    (width,    height)           = areaSize (area state)
    (newWidth, newHeight)        = areaSize newArea
    canvasArea                   = bounds (canvas state)
    
    newArea       = ((minX + dminX, minY + dminY), (maxX + dmaxX, maxY + dmaxY))
    newCanvasArea = canvasArea `unionArea` newArea

    newCanvas
      | newArea `containedWithin` canvasArea
      = canvas state
      | otherwise
      = array newCanvasArea [ (pos, if inRange canvasArea pos 
                                    then canvas state ! pos 
                                    else transparent)
                            | pos <- range newCanvasArea]


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
