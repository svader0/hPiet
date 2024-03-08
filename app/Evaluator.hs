module Evaluator where

import Data.Stack
import Load

step :: PietProgram -> Direction -> Stack -> CodelChooser -> (PietProgram, Direction, Stack, CodelChooser)