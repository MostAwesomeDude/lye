import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.STRef
import Text.Parsers.Frisby
import Text.Parsers.Frisby.Char

type Duration = Int

data Marker = Relative
    deriving (Eq, Show)

data Accidental = Flat | Sharp
    deriving (Eq, Show)

data Octave = OctaveUp | OctaveDown
    deriving (Eq, Show)

data Diatonic = C | D | E | F | G | A | B
    deriving (Eq, Show)

data Pitch = Pitch Diatonic [Accidental]
    deriving (Eq, Show)

data Note = Note Pitch Duration
    deriving (Eq, Show)

type Chord = [Note]

-- | Parse a string into an arbitrary object, consuming any leading
-- | whitespace.
token :: String -> a -> P s a
token s m = optional (many space) ->> (text s ##> m)

-- | Parse a series of digits into an Int.
integer :: P s Int
integer = let f = foldl1 (\x y -> x * 10 + y) . map digitToInt
    in many1 digit ## f

accidentals :: P s [Accidental]
accidentals = many $ (text "es" ##> Flat) // (text "is" ##> Sharp)

octaves :: P s [Octave]
octaves = many $ (char '\'' ##> OctaveUp) // (char ',' ##> OctaveDown)

-- | Parse a duration of note length and number of dots.
duration :: P s (Int, Int)
duration = integer <> (many (char '.') ## length)

-- | "Undot" a duration tuple into a single duration.
-- | XXX could do without ST?
undot :: (Int, Int) -> Int
undot (size, dots) = runST $ do
    duration <- newSTRef $ 480 `div` size
    dotted <- newSTRef $ 480 `div` size
    replicateM_ dots $ do
        modifySTRef dotted $ (`div` 2)
        dot <- readSTRef dotted
        modifySTRef duration $ (+ dot)
    readSTRef duration

diatonic :: P s Diatonic
diatonic = choice [ token "c" C
                  , token "d" D
                  , token "e" E
                  , token "f" F
                  , token "g" G
                  , token "a" A
                  , token "b" B ]

pitch :: P s Pitch
pitch = diatonic <> accidentals ## uncurry Pitch
