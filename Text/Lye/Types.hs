{-# LANGUAGE DeriveDataTypeable #-}
module Text.Lye.Types where

import Data.Data
import Data.Ratio

type Fraction = Ratio Integer

data Accidental = Flat | Sharp
    deriving (Show, Data, Typeable)

data Octave = OctaveDown | OctaveUp
    deriving (Show, Data, Typeable)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show, Data, Typeable)

data Expression = Chord [Expression]
                | Drums Expression
                | Duration Integer Integer
                | Music [Expression]
                | RawNote Char [Accidental] [Octave] (Maybe Expression)
                | Relative Char [Octave] Expression
                | Rest (Maybe Expression)
                | SciNote Integer Integer
                | Times Fraction Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show, Data, Typeable)
