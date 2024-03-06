{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}
module Load where

import Codec.Picture as Pic

-- Data type to define a Piet program
-- as a Grid of colors, and a pointer
-- to the current position in the grid
data PietProgram = PietProgram
  { grid :: [[PixelRGB8]],
    pointer :: (Int, Int)
  }

-- using JuicyPixels to load images
loadImage :: FilePath -> IO (PietProgram)
loadImage path = do
  image <- Pic.readImage path
  putStrLn $ "Loading image: " ++ path
  case image of
    Left err -> putStrLn err
    Right img -> do
