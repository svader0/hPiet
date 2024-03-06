module Main where

import Load (loadImage)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get the argument from the command line
  args <- getArgs
  if null args
    then putStrLn "No arguments"
    else loadImage (head args)