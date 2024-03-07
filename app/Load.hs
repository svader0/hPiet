{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}
module Load where

import Codec.Picture
import Codec.Picture.Types
import Data.Maybe (fromJust, isJust, listToMaybe)

data PietColor
  = LightRed
  | NormalRed
  | DarkRed
  | LightYellow
  | NormalYellow
  | DarkYellow
  | LightGreen
  | NormalGreen
  | DarkGreen
  | LightCyan
  | NormalCyan
  | DarkCyan
  | LightBlue
  | NormalBlue
  | DarkBlue
  | LightMagenta
  | NormalMagenta
  | DarkMagenta
  | Black
  | White
  deriving (Show, Enum, Bounded, Eq)

data Direction = Right | Down | Left | Up

type PietProgram = [[PietColor]]

-- lookup function
colorToRGB :: PietColor -> PixelRGB8
colorToRGB color = case color of
  LightRed -> PixelRGB8 255 192 192
  NormalRed -> PixelRGB8 255 0 0
  DarkRed -> PixelRGB8 192 0 0
  LightYellow -> PixelRGB8 255 255 192
  NormalYellow -> PixelRGB8 255 255 0
  DarkYellow -> PixelRGB8 192 192 0
  LightGreen -> PixelRGB8 192 255 192
  NormalGreen -> PixelRGB8 0 255 0
  DarkGreen -> PixelRGB8 0 192 0
  LightCyan -> PixelRGB8 192 255 255
  NormalCyan -> PixelRGB8 0 255 255
  DarkCyan -> PixelRGB8 0 192 192
  LightBlue -> PixelRGB8 192 192 255
  NormalBlue -> PixelRGB8 0 0 255
  DarkBlue -> PixelRGB8 0 0 192
  LightMagenta -> PixelRGB8 255 192 255
  NormalMagenta -> PixelRGB8 255 0 255
  DarkMagenta -> PixelRGB8 192 0 192
  Black -> PixelRGB8 0 0 0
  White -> PixelRGB8 255 255 255

-- Reverse lookup function
rgbToPietColor :: PixelRGB8 -> Maybe PietColor
rgbToPietColor rgb = listToMaybe [color | color <- [minBound .. maxBound], colorToRGB color == rgb]

loadImage :: FilePath -> IO (Maybe PietProgram)
loadImage path = do
  image <- readImage path
  case image of
    Prelude.Left err -> return Nothing
    Prelude.Right img -> return $ Just $ imageToPietProgram img

imageToPietProgram :: DynamicImage -> PietProgram
imageToPietProgram img = pixels
  where
    img' = convertRGB8 img
    pixels = buildGrid img'

buildGrid :: Image PixelRGB8 -> PietProgram
-- Note: It's OK to use fromJust here because we've already checked that all the colors are valid by this point.
buildGrid img = [[fromJust $ rgbToPietColor $ pixelAt img x y | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
  where
    width = imageWidth img
    height = imageHeight img

checkColors :: PietProgram -> Bool
checkColors = all (all (isJust . (rgbToPietColor . colorToRGB)))
