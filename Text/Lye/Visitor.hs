module Text.Lye.Visitor where

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
