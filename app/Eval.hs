module Eval where

import Load
import Types

-- Basic stack operations
push :: Int -> Stack -> Stack
push = (:)

pop :: Stack -> (Maybe Int, Stack)
pop [] = (Nothing, [])
pop (x : xs) = (Just x, xs)

checkBounds :: PietProgram -> Position -> Bool
checkBounds prog (x, y) = x >= 0 && y >= 0 && x < length prog && y < length (head prog)

-- Move our position in the direction of the direction pointer
moveDirection :: DirectionPointer -> Position -> Position
moveDirection DRight (x, y) = (x + 1, y)
moveDirection DDown (x, y) = (x, y + 1)
moveDirection DLeft (x, y) = (x - 1, y)
moveDirection DUp (x, y) = (x, y - 1)

getLightnessChange :: Lightness -> Lightness -> Int
getLightnessChange Normal Normal = 0
getLightnessChange Normal Light = 1
getLightnessChange Normal Dark = -1
getLightnessChange Light Normal = -1
getLightnessChange Light Light = 0
getLightnessChange Light Dark = -2
getLightnessChange Dark Normal = 1
getLightnessChange Dark Light = 2
getLightnessChange Dark Dark = 0

-- Get the change in hue between two hues
-- Should be between 0 and 5
getHueChange :: Hue -> Hue -> Int
getHueChange hue1 hue2
  | hue1 == hue2 = 0
  | hue1 == Red && hue2 == Yellow = 1
  | hue1 == Red && hue2 == Magenta = 2
  | hue1 == Yellow && hue2 == Red = 5
  | hue1 == Yellow && hue2 == Green = 1
  | hue1 == Green && hue2 == Yellow = 5
  | hue1 == Green && hue2 == Cyan = 1
  | hue1 == Cyan && hue2 == Green = 5
  | hue1 == Cyan && hue2 == Blue = 1
  | hue1 == Blue && hue2 == Cyan = 5
  | hue1 == Blue && hue2 == Magenta = 1
  | hue1 == Magenta && hue2 == Blue = 5
  | hue1 == Magenta && hue2 == Red = 1
  | hue1 == Red && hue2 == Green = 3
  | hue1 == Red && hue2 == Cyan = 4
  | hue1 == Yellow && hue2 == Magenta = 3
  | hue1 == Yellow && hue2 == Blue = 4
  | hue1 == Green && hue2 == Red = 3
  | hue1 == Green && hue2 == Magenta = 4
  | hue1 == Cyan && hue2 == Red = 3
  | hue1 == Cyan && hue2 == Yellow = 4
  | hue1 == Blue && hue2 == Yellow = 3
  | hue1 == Blue && hue2 == Green = 4
  | hue1 == Magenta && hue2 == Green = 3
  | hue1 == Black || hue2 == Black = 0
  | otherwise = 0
