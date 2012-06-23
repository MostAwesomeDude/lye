module Text.Lye.Parser where

import Control.Applicative
import Data.Ratio
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Prim (decimal)
import Text.Trifecta.Parser.Token.Combinators
import Text.Trifecta.Highlight.Prim

import Text.Lye.Types

-- From edwardk's stash-o-stuff.
infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
    a <- ma
    return $! f a

lexstr :: MonadParser m => String -> m String
lexstr = lexeme . string

parseNumber :: MonadParser m => m Integer
parseNumber = highlight Number (lexeme decimal) <?> "number"

parseFraction :: MonadParser m => m Fraction
parseFraction = (%) <$!>
    parseNumber
    <* char '/'
    <*> parseNumber
    <?> "fraction"

parseDots :: MonadParser m => m Integer
parseDots = do
    ds <- lexeme $ many (oneOf ".")
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
parsePitch = spaces
    *> highlight ReservedOperator (oneOf "abcdefg") <?> "pitch"

parseRest :: MonadParser m => m Char
parseRest = highlight ReservedOperator (oneOf "r") <?> "rest"

parseMusicExpr :: MonadParser m => m Expression
parseMusicExpr = Music <$!> braces (many parseExpr)

parseNoteExpr :: MonadParser m => m Expression
parseNoteExpr = lexeme $ RawNote <$!>
    parsePitch
    <*> many parseAccidental
    <*> many parseOctave
    <*> optional parseDuration

parseRelativeExpr :: MonadParser m => m Expression
parseRelativeExpr = do
    lexstr "\\relative"
    pitch <- parsePitch
    many parseAccidental
    octaves <- lexeme $ many parseOctave
    expr <- parseExpr
    return $! Relative pitch octaves expr

parseRestExpr :: MonadParser m => m Expression
parseRestExpr = do
    parseRest
    duration <- optional parseDuration
    return $! Rest duration

parseExpr :: MonadParser m => m Expression
parseExpr = choice
    [ parseMusicExpr
    , parseNoteExpr
    , parseRelativeExpr
    , parseRestExpr
    , Chord <$!> angles (many parseNoteExpr)
    , Times <$!> (lexstr "\\times" *> parseFraction) <*> parseMusicExpr ]
