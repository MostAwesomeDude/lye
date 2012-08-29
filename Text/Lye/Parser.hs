module Text.Lye.Parser where

import Control.Applicative
import Data.Ratio
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight

import Text.Lye.Types

-- From edwardk's stash-o-stuff.
infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
    a <- ma
    return $! f a

-- | Might need to be split up if we ever care about the return value.
slashSymbol :: (Monad m, TokenParsing m) => String -> m ()
slashSymbol s = highlight ReservedIdentifier $ symbol ('\\':s) >> return ()

number :: (TokenParsing m, Monad m) => m Integer
number = highlight Number natural

fraction :: (Monad m, TokenParsing m) => m Rational
fraction = (%) <$!> number <* char '/' <*> number <?> "number"

dots :: (Monad m, TokenParsing m) => m Integer
dots = fromIntegral . length <$!> many (char '.') <* whiteSpace <?> "dots"

dotsToRatio :: Integer -> Rational
dotsToRatio dots = 2 - (1 % (2 ^ dots))

duration :: (Monad m, TokenParsing m) => m Duration
duration = do
    len <- number
    dots <- dots
    let ratio = 1 % len
    return $ Duration $! ratio * (dotsToRatio dots)

octaveDown :: (Monad m, CharParsing m) => m Octave
octaveDown = char ',' >> return OctaveDown

octaveUp :: (Monad m, CharParsing m) => m Octave
octaveUp = char '\'' >> return OctaveUp

octave :: (Monad m, CharParsing m) => m Octave
octave = octaveDown <|> octaveUp <?> "octave"

flat :: (Monad m, CharParsing m) => m Accidental
flat = string "es" >> return Flat

sharp :: (Monad m, CharParsing m) => m Accidental
sharp = string "is" >> return Sharp

accidental :: (Monad m, CharParsing m) => m Accidental
accidental = flat <|> sharp <?> "accidental"

pitch :: (Monad m, TokenParsing m) => m Pitch
pitch = let
    c2i c = toEnum $ fromEnum c - 97
    _p = oneOf "abcdefg" >>= return . c2i
    in spaces *> highlight ReservedOperator _p <?> "pitch"

rest :: TokenParsing m => m Char
rest = highlight ReservedOperator (char 'r') <?> "rest"

key :: (Monad m, TokenParsing m) => m Key
key = let
    major = do
        p <- pitch
        ma <- optional accidental
        whiteSpace
        slashSymbol "major"
        return $! Major p ma
    minor = do
        p <- pitch
        ma <- optional accidental
        whiteSpace
        slashSymbol "minor"
        return $! Minor p ma
    in do
        slashSymbol "key"
        choice [major, minor]

time :: (Monad m, TokenParsing m) => m Directive
time = slashSymbol "time" >> TimeDir <$!> fraction

dirExpr :: (Monad m, TokenParsing m) => m Expression
dirExpr = let
    _pk = KeyDir <$> key
    in DirectiveExpr <$!> choice [_pk, time]

drumsExpr :: (Monad m, TokenParsing m) => m Expression
drumsExpr = do
    slashSymbol "drums"
    Drums <$!> expr

markerExpr :: (Monad m, TokenParsing m) => m Expression
markerExpr = let
    f s m = do
        symbol s
        return $! MarkerExpr Measure
    -- Sorry!
    c = flip (flip uncurry . unzip) $ zipWith f
    in choice . c $ [("|", Measure), ("(", OpenSlur), (")", CloseSlur)]

musicExpr :: (Monad m, TokenParsing m) => m Expression
musicExpr = Music <$!> braces (many expr)

noteExpr :: (Monad m, TokenParsing m) => m Expression
noteExpr = ParsedNote <$!>
    pitch
    <*> many accidental
    <*> many octave
    <*> optional duration
    <*  whiteSpace

relativeExpr :: (Monad m, TokenParsing m) => m Expression
relativeExpr = do
    slashSymbol "relative"
    pitch <- pitch
    many accidental
    octaves <- many octave
    whiteSpace
    expr <- expr
    return $! Relative pitch octaves expr

restExpr :: (Monad m, TokenParsing m) => m Expression
restExpr = do
    rest
    duration <- optional duration
    return $! ParsedRest duration

timesExpr :: (Monad m, TokenParsing m) => m Expression
timesExpr = Times <$!> (slashSymbol "times" *> fraction) <*> musicExpr

expr :: (Monad m, TokenParsing m) => m Expression
expr = choice
    -- "\times" must come before "\time", so timesExpr needs to come before
    -- dirExpr. A similar issue will come up when we add voices.
    [ timesExpr
    , drumsExpr
    , markerExpr
    , musicExpr
    , noteExpr
    , relativeExpr
    , restExpr
    , Chord <$!> angles (many noteExpr)
    , dirExpr ]

exprs :: (Monad m, TokenParsing m) => m [Expression]
exprs = many expr

fullParse :: (Monad m, TokenParsing m) => m [Expression]
fullParse = between whiteSpace eof exprs
