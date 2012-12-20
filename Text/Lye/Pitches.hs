module Text.Lye.Pitches where

import Text.Lye.Types

accidentalsToInt :: [Accidental] -> Int
accidentalsToInt = let
    f x = case x of
        Sharp -> 1
        Flat -> -1
    in sum . map f

octavesToInt :: [Octave] -> Int
octavesToInt = let
    f x = case x of
        OctaveUp -> 12
        OctaveDown -> -12
    in sum . map f

pitchToNumber :: Pitch -> [Accidental] -> [Octave] -> Int
pitchToNumber c as os = let
    a = accidentalsToInt as
    o = octavesToInt os
    x = o + a
    y = case c of
        C -> 48
        D -> 50
        E -> 52
        F -> 53
        G -> 55
        A -> 57
        B -> 59
    in x + y

-- | Indicate which direction, if any, should be used to adjust the
--   transition from the first to the second pitch.
transposeDirection :: Pitch -> Pitch -> Maybe Octave
transposeDirection p q
    | q > p && (q' - p' > 3) = Just OctaveDown
    | q < p && (p' - q' > 3) = Just OctaveUp
    | otherwise              = Nothing
    where
    p' = fromEnum p
    q' = fromEnum q
