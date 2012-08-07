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

parseFraction :: MonadParser m => m Rational
parseFraction = (%) <$!>
    parseNumber
    <* char '/'
    <*> parseNumber
    <?> "fraction"

parseDots :: MonadParser m => m Integer
parseDots = do
    ds <- lexeme $ many (oneOf ".")
    return $ fromIntegral (length ds)

dotsToRatio :: Integer -> Rational
dotsToRatio dots = 2 - (1 % (2 ^ dots))

parseDuration :: MonadParser m => m Duration
parseDuration = do
    length <- parseNumber
    dots <- parseDots
    let ratio = 1 % length
    return $ Duration $! ratio * (dotsToRatio dots)

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

parsePitch :: MonadParser m => m Pitch
parsePitch = let
    c2i c = toEnum $ fromEnum c - 97
    _p = oneOf "abcdefg" >>= return . c2i
    in spaces *> highlight ReservedOperator _p <?> "pitch"

parseRest :: MonadParser m => m Char
parseRest = highlight ReservedOperator (oneOf "r") <?> "rest"

parseKey :: MonadParser m => m Key
parseKey = let
    major = do
        p <- parsePitch
        ma <- optional parseAccidental
        spaces
        lexstr "\\major"
        return $! Major p ma
    minor = do
        p <- parsePitch
        ma <- optional parseAccidental
        spaces
        lexstr "\\minor"
        return $! Minor p ma
    in do
        lexstr "\\key"
        choice [major, minor]

parseDirExpr :: MonadParser m => m Expression
parseDirExpr = DirectiveExpr <$!> KeyDir <$> parseKey

parseDrumsExpr :: MonadParser m => m Expression
parseDrumsExpr = do
    lexstr "\\drums"
    expr <- parseExpr
    return $! Drums expr

parseMusicExpr :: MonadParser m => m Expression
parseMusicExpr = Music <$!> braces (many parseExpr)

parseNoteExpr :: MonadParser m => m Expression
parseNoteExpr = lexeme $ ParsedNote <$!>
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
    return $! ParsedRest duration

parseExpr :: MonadParser m => m Expression
parseExpr = choice
    [ parseDirExpr
    , parseDrumsExpr
    , parseMusicExpr
    , parseNoteExpr
    , parseRelativeExpr
    , parseRestExpr
    , Chord <$!> angles (many parseNoteExpr)
    , Times <$!> (lexstr "\\times" *> parseFraction) <*> parseMusicExpr ]

parseExprs :: MonadParser m => m [Expression]
parseExprs = many parseExpr
