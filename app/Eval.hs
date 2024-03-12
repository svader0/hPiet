module Eval where

import Data.Maybe (fromJust)
import Load
import Stack
import Types

checkBounds :: PietProgram -> Position -> Bool
checkBounds prog (x, y) = x >= 0 && y >= 0 && x < length prog && y < length (head prog)

-- Attempt to move the DP in the current direction, considering boundaries
moveDP :: PietState -> PietState
moveDP execCtx = execCtx {position = newPosition}
  where
    newPosition = case dp execCtx of
      DLeft -> (max 0 (fst (position execCtx) - 1), snd (position execCtx))
      DRight -> (min (width - 1) (fst (position execCtx) + 1), snd (position execCtx))
      DUp -> (fst (position execCtx), max 0 (snd (position execCtx) - 1))
      DDown -> (fst (position execCtx), min (height - 1) (snd (position execCtx) + 1))
    width = length (head (program execCtx))
    height = length (program execCtx)

-- Example of integrating a stack operation into the ExecutionContext
executeCommand :: PietState -> PietState
executeCommand execCtx = undefined

-- Function to calculate the hue and lightness change between two pixels
calculateTransition :: PietColor -> PietColor -> ColorChange
calculateTransition prevColor nextColor = (hueChange, lightnessChange)
  where
    hueChange = fromEnum (fst nextColor) - fromEnum (fst prevColor)
    lightnessChange = fromEnum (snd nextColor) - fromEnum (snd prevColor)

-- Map a hue and lightness change to a Piet command
mapTransitionToCommand :: (HueChange, LightnessChange) -> Maybe Command
mapTransitionToCommand (hueChange, lightnessChange) = undefined