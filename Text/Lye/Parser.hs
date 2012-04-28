module Text.Lye.Parser where

import Control.Applicative
import Data.Ratio
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Prim (decimal)
import Text.Trifecta.Parser.Token.Combinators
import Text.Trifecta.Highlight.Prim

type Fraction = Ratio Integer

data Accidental = Flat | Sharp
    deriving (Show)

data Octave = OctaveDown | OctaveUp
    deriving (Show)

data Marker = EndVoice
            | Measure
            | Partial
            | Tie
    deriving (Show)

data Expression = Chord [Expression]
                | Drums Expression
                | Duration Integer Integer
                | Music [Expression]
                | RawNote Char [Accidental] [Octave] (Maybe Expression)
                | Rest Integer
                | SciNote Integer Integer
                | Times Fraction Expression
                | Voice [Expression]
                | Voices [Expression]
    deriving (Show)

-- From edwardk's stash-o-stuff.
infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
    a <- ma
    return $! f a

parseNumber :: MonadParser m => m Integer
parseNumber = highlight Number decimal <?> "number"

parseFraction :: MonadParser m => m Fraction
parseFraction = (%) <$!> parseNumber <* char '/' <*> parseNumber <?> "fraction"

parseDots :: MonadParser m => m Integer
parseDots = do
    ds <- many (oneOf ".")
    return $ fromIntegral (length ds)

parseDuration :: MonadParser m => m Expression
parseDuration = Duration <$!> parseNumber <*> parseDots <?> "duration"

_char2Octave :: Char -> Octave
_char2Octave c = case c of
    '\'' -> OctaveUp
    ',' -> OctaveDown

parseOctave :: MonadParser m => m Octave
parseOctave = do
    c <- oneOf "',"
    return $ _char2Octave c

parseFlat :: MonadParser m => m Accidental
parseFlat = string "es" >> return Flat

parseSharp :: MonadParser m => m Accidental
parseSharp = string "is" >> return Sharp

parseAccidental :: MonadParser m => m Accidental
parseAccidental = parseFlat <|> parseSharp

parsePitch :: MonadParser m => m Char
parsePitch = highlight ReservedOperator (oneOf "abcdefg") <?> "pitch"

parseNoteExpr :: MonadParser m => m Expression
parseNoteExpr = RawNote <$!>
    parsePitch
    <*> many parseAccidental
    <*> many parseOctave
    <*> optional parseDuration
    <* someSpace

parseExpr :: MonadParser m => m Expression
parseExpr = choice
    [ parseNoteExpr
    , Music <$!> braces (many parseExpr) ]
