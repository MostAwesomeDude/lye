{-# LANGUAGE DeriveDataTypeable #-}
module Text.Lye.Types where

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

data Key = Major Pitch | Minor Pitch
    deriving (Show, Data, Typeable)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show, Data, Typeable)

data Expression = Chord [Expression]
                | Drums Expression
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

pitchToNumber :: Pitch -> [Accidental] -> [Octave] -> Integer
pitchToNumber c as os = let
    a = accidentalsToInt as
    o = octavesToInt os
    x = o * 12 + a
    y = case c of
        C -> 48
        D -> 50
        E -> 52
        F -> 53
        G -> 55
        A -> 57
        B -> 59
    in x + y

-- | Indicate which direction is nearer for transposition or relative
--   assignment.
transposeDirection :: Pitch -> Pitch -> Octave
transposeDirection p q = let
    p' = fromEnum p
    q' = fromEnum q
    diff = q' - p' `mod` 7
    in if diff > 3 then OctaveDown else OctaveUp
