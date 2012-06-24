{-# LANGUAGE DeriveDataTypeable #-}
module Text.Lye.Types where

import Data.Data
import Data.Ratio

type Fraction = Ratio Integer

data Accidental = Flat | Sharp
    deriving (Show, Data, Typeable)

data Octave = OctaveDown | OctaveUp
    deriving (Show, Data, Typeable)

data Duration = ParsedDuration Integer Integer
              | Duration (Ratio Integer)
    deriving (Show, Data, Typeable)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show, Data, Typeable)

data Expression = Chord [Expression]
                | Drums Expression
                | Music [Expression]
                | Note Char [Accidental] [Octave] Duration
                | ParsedNote Char [Accidental] [Octave] (Maybe Duration)
                | ParsedRest (Maybe Duration)
                | Relative Char [Octave] Expression
                | Rest Duration
                | SciNote Integer Integer
                | Times Fraction Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show, Data, Typeable)
