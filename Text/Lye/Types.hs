{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Text.Lye.Types where

import Control.Lens
import Data.Data
import Data.Ratio()

data Accidental = Flat | Sharp
    deriving (Show, Eq, Data, Typeable)

data Octave = OctaveDown | OctaveUp
    deriving (Show, Eq, Data, Typeable)

data Pitch = C | D | E | F | G | A | B
    deriving (Show, Eq, Ord, Enum, Data, Typeable)

data Drum = Bass | SideStick | Snare | LowFloorTom | ClosedHat | HighFloorTom
          | PedalHat | LowTom | OpenHat | LowMidTom | HighMidTom | HighTom
          | Cowbell | LowTimbale | HighTimbale | MutedTriangle | Triangle
    deriving (Show, Eq, Ord, Data, Typeable)

instance Enum Drum where
    fromEnum Bass = 36
    fromEnum SideStick = 37
    fromEnum Snare = 40
    fromEnum LowFloorTom = 41
    fromEnum ClosedHat = 42
    fromEnum HighFloorTom = 43
    fromEnum PedalHat = 44
    fromEnum LowTom = 45
    fromEnum OpenHat = 46
    fromEnum LowMidTom = 47
    fromEnum HighMidTom = 48
    fromEnum HighTom = 50
    fromEnum Cowbell = 56
    fromEnum LowTimbale = 65
    fromEnum HighTimbale = 66
    fromEnum MutedTriangle = 80
    fromEnum Triangle = 81

    toEnum 36 = Bass
    toEnum 37 = SideStick
    toEnum 40 = Snare
    toEnum 41 = LowFloorTom
    toEnum 42 = ClosedHat
    toEnum 43 = HighFloorTom
    toEnum 44 = PedalHat
    toEnum 45 = LowTom
    toEnum 46 = OpenHat
    toEnum 47 = LowMidTom
    toEnum 48 = HighMidTom
    toEnum 50 = HighTom
    toEnum 56 = Cowbell
    toEnum 65 = LowTimbale
    toEnum 66 = HighTimbale
    toEnum 80 = MutedTriangle
    toEnum 81 = Triangle

newtype Duration = Duration Rational
    deriving (Show, Eq, Data, Typeable, Num)

instance Plated Duration

data Key = Major Pitch (Maybe Accidental)
         | Minor Pitch (Maybe Accidental)
    deriving (Show, Eq, Data, Typeable)

data Directive = KeyDir Key
               | TimeDir Rational
    deriving (Show, Eq, Data, Typeable)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
            | OpenSlur
            | CloseSlur
    deriving (Show, Eq, Data, Typeable)

data Expression = Chord [Expression]
                | DirectiveExpr Directive
                | DrumNote Drum Duration
                | Drums Expression
                | MarkerExpr Marker
                | Music [Expression]
                | Note Pitch [Accidental] [Octave] Duration
                | ParsedDrumNote Drum (Maybe Duration)
                | ParsedNote Pitch [Accidental] [Octave] (Maybe Duration)
                | ParsedRest (Maybe Duration)
                | Relative Pitch [Octave] Expression
                | Rest Duration
                | SciNote Int Duration
                | Times Rational Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show, Eq, Data, Typeable)

instance Plated Expression

data Notes a = NoteOn Int Int Int Int a
             | NoteOff Int Int Int Int a
    deriving (Show, Eq, Functor, Data, Typeable)

data Annotated = Annotated { _aExpression :: Expression
                           , _aKey :: Key
                           , _aTime :: Rational }
    deriving (Show, Data, Typeable)

makeLenses ''Annotated
