module Text.Lye.Visitor where

import Data.Ratio
import Text.Lye.Types

import Data.Generics.Uniplate.Data

inlineDrums :: Expression -> Expression
inlineDrums = let
    f (Drums expr) = expr
    f x = x
    in transform f
