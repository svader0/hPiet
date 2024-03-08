module Types where

type DirectionPointer = Direction

type CodelChooser = Direction

type Position = (Int, Int)

type Stack = [Int]

data Commands = Push | Pop | Add | Subtract | Multiply | Divide | Modulo | Not | Greater | Pointer | Switch | Duplicate | Roll | InNumber | InChar | OutNumber | OutChar deriving (Eq, Show)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White deriving (Eq, Show, Enum, Bounded)

data Lightness = Light | Normal | Dark deriving (Eq, Show, Enum, Bounded)

type PietColor = (Hue, Lightness)

data Direction = DRight | DDown | DLeft | DUp

type PietProgram = [[PietColor]]

type MaybeE a = Either String a

data ColorChange = ColorChange
  { hueChange :: Int, -- -2, -1, 0, 1, 2 (Previous, Next in the hue cycle)
    lightnessChange :: Int -- -1, 0, 1 (Darker, Same, Lighter)
  }
  deriving (Eq, Show)

data PietState = PietState
  { dp :: DirectionPointer,
    cc :: CodelChooser,
    stack :: Stack,
    position :: Position,
    color :: PietColor,
    commands :: [Commands],
    program :: PietProgram
  }
