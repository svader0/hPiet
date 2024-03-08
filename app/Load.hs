{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}
module Load where

import Codec.Picture
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Types

-- lookup function
colorToRGB :: PietColor -> PixelRGB8
colorToRGB color = case color of
  (Red, Light) -> PixelRGB8 255 192 192
  (Red, Normal) -> PixelRGB8 255 0 0
  (Red, Dark) -> PixelRGB8 192 0 0
  (Yellow, Light) -> PixelRGB8 255 255 192
  (Yellow, Normal) -> PixelRGB8 255 255 0
  (Yellow, Dark) -> PixelRGB8 192 192 0
  (Green, Light) -> PixelRGB8 192 255 192
  (Green, Normal) -> PixelRGB8 0 255 0
  (Green, Dark) -> PixelRGB8 0 192 0
  (Cyan, Light) -> PixelRGB8 192 255 255
  (Cyan, Normal) -> PixelRGB8 0 255 255
  (Cyan, Dark) -> PixelRGB8 0 192 192
  (Blue, Light) -> PixelRGB8 192 192 255
  (Blue, Normal) -> PixelRGB8 0 0 255
  (Blue, Dark) -> PixelRGB8 0 0 192
  (Magenta, Light) -> PixelRGB8 255 192 255
  (Magenta, Normal) -> PixelRGB8 255 0 255
  (Magenta, Dark) -> PixelRGB8 192 0 192
  (Black, _) -> PixelRGB8 0 0 0
  (White, _) -> PixelRGB8 255 255 255

isPietColor :: PixelRGB8 -> Bool
isPietColor rgb = isJust $ rgbToPietColor rgb

rgbToPietColor :: PixelRGB8 -> Maybe PietColor
rgbToPietColor rgb = find (\c -> colorToRGB c == rgb) ([(h, l) | h <- [minBound .. maxBound], l <- [minBound .. maxBound]])

loadImage :: FilePath -> IO (MaybeE PietProgram)
loadImage path = do
  image <- readImage path
  case image of
    Prelude.Left err -> return $ Prelude.Left err
    Prelude.Right img -> return $ maybe (Prelude.Left " Image contains colors that are not valid in the Piet language.\n\tSee https://www.dangermouse.net/esoteric/piet.html for more information.") Prelude.Right $ imageToPietProgram img

imageToPietProgram :: DynamicImage -> Maybe PietProgram
imageToPietProgram img =
  if isJust $ buildGrid $ convertRGB8 img
    then buildGrid $ convertRGB8 img
    else Nothing
  where
    buildGrid :: Image PixelRGB8 -> Maybe PietProgram
    buildGrid img' =
      if isJust $ rgbToPietColor $ pixelAt img' 0 0
        then Just $ map (\y -> map (\x -> fromJust $ rgbToPietColor $ pixelAt img' x y) [0 .. imageWidth img' - 1]) [0 .. imageHeight img' - 1]
        else Nothing
