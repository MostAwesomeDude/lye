module Text.Lye.Parser where

-- A note about general theory here. What would be really useful for us is if
-- we could follow the parsing style where there is no leading whitespace. In
-- order to do this, we must *never* parse leading whitespace on any data, and
-- instead we must be vigilant in mopping up trailing whitespace anywhere that
-- it is legal. This is only difficult because nearly every expression in
-- Lilypond permits leading whitespace, due to their parser's "design."

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
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
slashSymbol s = highlight ReservedIdentifier $ void $ symbol ('\\':s)

number :: (TokenParsing m, Monad m) => m Integer
number = highlight Number natural

fraction :: (Monad m, TokenParsing m) => m Rational
fraction = (%) <$!> number <* char '/' <*> number <?> "number"

dots :: (Monad m, TokenParsing m) => m Integer
dots = fromIntegral . length <$!> many (char '.') <?> "dots"

dotsToRatio :: Integer -> Rational
dotsToRatio ds = 2 - (1 % (2 ^ ds))

duration :: (Monad m, TokenParsing m) => m Duration
duration = do
    len <- number
    ds <- dots
    let ratio = 1 % len
    return $ Duration $! ratio * dotsToRatio ds

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
pitch = highlight ReservedOperator p <?> "pitch"
    where
    p = choice $ M.foldlWithKey folder [] pmap
    folder ps key value = (char key >> return value) : ps
    pmap = M.fromList [ ('c', C)
                      , ('d', D)
                      , ('e', E)
                      , ('f', F)
                      , ('g', G)
                      , ('a', A)
                      , ('b', B) ]

drum :: (Monad m, TokenParsing m) => m Drum
drum = highlight ReservedOperator d <?> "drum"
    where
    d = choice $ M.foldlWithKey folder [] dmap
    folder ds key value = (string key >> return value) : ds
    dmap = M.fromList [ ("bd", Bass)
                      , ("ss", SideStick)
                      , ("sn", Snare)
                      , ("tomfl", LowFloorTom)
                      , ("hhc", ClosedHat)
                      , ("tomfh", HighFloorTom)
                      , ("hhp", PedalHat)
                      , ("toml", LowTom)
                      , ("hho", OpenHat)
                      , ("tomml", LowMidTom)
                      , ("tommh", HighMidTom)
                      , ("tomh", HighTom)
                      , ("cb", Cowbell)
                      , ("timl", LowTimbale)
                      , ("timh", HighTimbale)
                      , ("trim", MutedTriangle)
                      , ("tri", Triangle)
                      , ("trio", Triangle) ]

rest :: TokenParsing m => m Char
rest = highlight ReservedOperator (char 'r') <?> "rest"

key :: (Monad m, TokenParsing m) => m Key
key = let
    _k = do
        p <- pitch
        ma <- optional accidental
        whiteSpace
        return (p, ma)
    major = do
        (p, ma) <- _k
        slashSymbol "major"
        return $! Major p ma
    minor = do
        (p, ma) <- _k
        slashSymbol "minor"
        return $! Minor p ma
    in do
        slashSymbol "key"
        choice [major, minor]

time :: (Monad m, TokenParsing m) => m Directive
time = slashSymbol "time" >> TimeDir <$!> fraction

chordExpr :: (Monad m, TokenParsing m) => m Expression
chordExpr = Chord <$!> angles (many noteExpr)

dirExpr :: (Monad m, TokenParsing m) => m Expression
dirExpr = let
    _pk = KeyDir <$> key
    in DirectiveExpr <$!> choice [_pk, time]

drumsExpr :: (Monad m, TokenParsing m) => m Expression
drumsExpr = slashSymbol "drums" >> Drums <$!> expr

markerExpr :: (Monad m, TokenParsing m) => m Expression
markerExpr = let
    f s m = symbol s >> MarkerExpr <$!> return m
    -- Sorry! So sorry! Maybe someday this could be cleaned up.
    c = flip (flip uncurry . unzip) $ zipWith f
    in choice . c $ [ ("|", Measure)
                    , ("(", OpenSlur)
                    , (")", CloseSlur)
                    , ("~", Tie) ]

musicExpr :: (Monad m, TokenParsing m) => m Expression
musicExpr = Music <$!> braces exprs

noteExpr :: (Monad m, TokenParsing m) => m Expression
noteExpr = ParsedNote <$!>
    pitch
    <*> many accidental
    <*> many octave
    <*> optional duration
    <*  whiteSpace

drumExpr :: (Monad m, TokenParsing m) => m Expression
drumExpr = ParsedDrumNote <$!> drum <*> optional duration <* whiteSpace

restExpr :: (Monad m, TokenParsing m) => m Expression
restExpr = rest >> ParsedRest <$!> optional duration <* whiteSpace

relativeExpr :: (Monad m, TokenParsing m) => m Expression
relativeExpr = do
    slashSymbol "relative"
    p <- pitch
    -- Discard accidentals. They are valid but don't affect relative
    -- scheduling.
    _ <- many accidental
    os <- many octave
    whiteSpace
    e <- expr
    return $! Relative p os e

timesExpr :: (Monad m, TokenParsing m) => m Expression
timesExpr = Times <$!> (slashSymbol "times" *> fraction) <*> musicExpr

voicesExpr :: (Monad m, TokenParsing m) => m Expression
voicesExpr = Voices <$!> between (symbol "<<") (symbol ">>") exprs

expr :: (Monad m, TokenParsing m) => m Expression
expr = choice
    -- "\times" must come before "\time", so timesExpr needs to come before
    -- dirExpr. Similarly, "<<" must come before "<", so voicesExpr needs to
    -- precede chordExpr.
    [ timesExpr
    , drumExpr
    , drumsExpr
    , markerExpr
    , musicExpr
    , noteExpr
    , relativeExpr
    , restExpr
    , voicesExpr
    , chordExpr
    , dirExpr ]

exprs :: (Monad m, TokenParsing m) => m [Expression]
exprs = many expr

fullParse :: (Monad m, TokenParsing m) => m [Expression]
fullParse = between whiteSpace eof exprs
