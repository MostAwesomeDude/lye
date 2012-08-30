module Text.Lye.Pitches where

import Text.Lye.Types

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
--   assignment. True means that the nearest one is an octave *down* and
--   should be adjusted accordingly.
transposeDirection :: Pitch -> Pitch -> Bool
transposeDirection p q = let
    p' = fromEnum p
    q' = fromEnum q
    diff = q' - p' `mod` 7
    in diff > 3
