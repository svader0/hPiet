module Main where

import Load
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get the argument from the command line
  args <- getArgs
  if null args
    then putStrLn "No arguments"
    else do
      -- Load the image
      let path = head args
      pietProgram <- loadImage path
      case pietProgram of
        Nothing -> putStrLn "Error loading image"
        Just p -> putStrLn $ "Image loaded" ++ show p
