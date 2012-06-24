module Text.Lye.Visitor where

import Control.Monad.State
import Data.Functor
import Data.Ratio
import Text.Lye.Types

import Data.Generics.Uniplate.Data

applyPeephole :: ([Expression] -> [Expression]) -> Expression -> Expression
applyPeephole f = let
    g (Chord es) = Chord $ f es
    g (Music es) = Music $ f es
    g (Voice es) = Voice $ f es
    g (Voices es) = Voices $ f es
    g e = e
    in transform g

inlineDrums :: Expression -> Expression
inlineDrums = let
    f (Drums expr) = Just expr
    f x = Nothing
    in rewrite f

flattenMusic :: Expression -> Expression
flattenMusic = let
    f (Music [x]) = Just x
    f x = Nothing
    in rewrite f

longestChord :: Expression -> Int
longestChord = let
    f (Chord xs) is = maximum $ length xs:is
    f x is = maximum $ 0:is
    in para f

-- | Apply durations to an expression.
--   Uses the State monad to move data across the AST, and manually recurses
--   in the correct manner.
applyDurations :: Expression -> Expression
applyDurations expr = let
    f (ParsedNote c a o (Just d)) = put d >> return (Note c a o d)
    f (ParsedNote c a o Nothing) = Note c a o <$> get
    f (ParsedRest (Just d)) = put d >> return (Rest d)
    f (ParsedRest Nothing) = Rest <$> get
    f x = return x
    recurser e = do
        e' <- f e
        descendM recurser e'
    initial = Duration (1 % 4)
    in evalState (recurser expr) initial

cleanDurations :: Expression -> Expression
cleanDurations = let
    f (ParsedDuration num dem) = Duration (num % dem)
    f x = x
    in transformBi f

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
