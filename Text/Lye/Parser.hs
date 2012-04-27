module Text.Lye.Parser where

import Control.Applicative
import Data.Ratio
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Token.Prim (decimal)
import Text.Trifecta.Highlight.Prim

type Fraction = Ratio Integer

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show)

data Expression = Chord [Expression]
                | Drums Expression
                | Duration Integer Integer
                | Music [Expression]
                | Note Integer Integer Integer Integer
                | Rest Integer
                | SciNote Integer Integer
                | Times Fraction Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show)

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
    a <- ma
    return $! f a

parseNumber :: MonadParser m => m Integer
parseNumber = highlight Number decimal <?> "number"

parseFraction :: MonadParser m => m Fraction
parseFraction = (%) <$!> parseNumber <* char '/' <*> parseNumber <?> "fraction"
