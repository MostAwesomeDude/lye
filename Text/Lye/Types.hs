{-# LANGUAGE DeriveDataTypeable #-}
module Text.Lye.Types where

import Data.Data
import Data.Ratio

data Accidental = Flat | Sharp
    deriving (Show, Data, Typeable)

data Octave = OctaveDown | OctaveUp
    deriving (Show, Data, Typeable)

data Duration = Duration Rational
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
                | Times Rational Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show, Data, Typeable)

accidentalsToInt :: [Accidental] -> Integer
accidentalsToInt = let
    f x = case x of
        Sharp -> 1
        Flat -> -1
    in sum . map f

octavesToInt :: [Octave] -> Integer
octavesToInt = let
    f x = case x of
        OctaveUp -> 12
        OctaveDown -> -12
    in sum . map f

pitchToNumber :: Char -> [Accidental] -> [Octave] -> Integer
pitchToNumber c as os = let
    a = accidentalsToInt as
    o = octavesToInt os
    x = o * 12 + a
    y = case c of
        'c' -> 48
        'd' -> 50
        'e' -> 52
        'f' -> 53
        'g' -> 55
        'a' -> 57
        'b' -> 59
    in x + y
