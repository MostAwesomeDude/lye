module Text.Lye.Visitor where

import Data.Ratio
import Text.Lye.Types

import Data.Generics.Uniplate.Data

inlineDrums :: Expression -> Expression
inlineDrums = let
    f (Drums expr) = expr
    f x = x
    in transform f

flattenMusic :: Expression -> Expression
flattenMusic = let
    f (Music [x]) = x
    f x = x
    in transform f

longestChord :: Expression -> Int
longestChord = let
    f (Chord xs) is = maximum $ length xs:is
    f x is = maximum $ 0:is
    in para f
