module Types where

type DirectionPointer = Direction

type CodelChooser = Direction

type Position = (Int, Int)

type Stack = [Int]

data Command = Push | Pop | Add | Subtract | Multiply | Divide | Modulo | Not | Greater | Pointer | Switch | Duplicate | Roll | InNumber | InChar | OutNumber | OutChar deriving (Eq, Show)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta | Black | White deriving (Eq, Show, Enum, Bounded)

data Lightness = Dark | Normal | Light deriving (Eq, Show, Enum, Bounded)

type PietColor = (Hue, Lightness)

type HueChange = Int

type LightnessChange = Int

type ColorChange = (HueChange, LightnessChange)

data Direction = DRight | DDown | DLeft | DUp

type PietProgram = [[PietColor]]

type MaybeE a = Either String a

data PietState = PietState
  { dp :: DirectionPointer,
    cc :: CodelChooser,
    stack :: Stack,
    position :: Position,
    color :: PietColor,
    commands :: [Command],
    program :: PietProgram
  }
