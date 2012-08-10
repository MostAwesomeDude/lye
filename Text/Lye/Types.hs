{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Text.Lye.Types where

import Control.Lens.TH
import Data.Data
import Data.Ratio

data Accidental = Flat | Sharp
    deriving (Show, Data, Typeable)

data Octave = OctaveDown | OctaveUp
    deriving (Show, Data, Typeable)

data Pitch = A | B | C | D | E | F | G
    deriving (Show, Eq, Ord, Enum, Data, Typeable)

data Duration = Duration Rational
    deriving (Show, Data, Typeable)

data Key = Major Pitch (Maybe Accidental)
         | Minor Pitch (Maybe Accidental)
    deriving (Show, Data, Typeable)

data Directive = KeyDir Key
               | TimeDir Rational
    deriving (Show, Data, Typeable)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show, Data, Typeable)

data Expression = Chord [Expression]
                | DirectiveExpr Directive
                | Drums Expression
                | MarkerExpr Marker
                | Music [Expression]
                | Note Pitch [Accidental] [Octave] Duration
                | ParsedNote Pitch [Accidental] [Octave] (Maybe Duration)
                | ParsedRest (Maybe Duration)
                | Relative Pitch [Octave] Expression
                | Rest Duration
                | SciNote Integer Integer
                | Times Rational Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show, Data, Typeable)

data Annotated = Annotated { _expression :: Expression
                           , _key :: Key
                           , _time :: Rational }
    deriving (Show, Data, Typeable)

-- makeLenses ''Annotated
