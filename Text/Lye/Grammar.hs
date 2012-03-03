import Control.Monad
import Control.Monad.ST
import Data.Char
import qualified Data.Map as M
import Data.STRef
import Text.Parsers.Frisby
import Text.Parsers.Frisby.Char

type Duration = Int

data Marker = Measure | Partial | Relative | Tie
    deriving (Eq, Show)

data Accidental = Flat | Sharp
    deriving (Eq, Show)

data Octave = OctaveUp | OctaveDown
    deriving (Eq, Show)

data Diatonic = C | D | Es | E | F | G | A | B | R
    deriving (Eq, Ord, Show)

data Pitch = PitchData Diatonic [Accidental] [Octave]
           | MIDIPitch Int
           | Rest
    deriving (Eq, Show)

data Note = Note Pitch (Maybe Duration)
    deriving (Eq, Show)

type Chord = [Note]

-- | Parse a string into an arbitrary object, consuming any leading
-- | whitespace.
token :: String -> a -> P s a
token s m = optional (many space) ->> (text s ##> m)

-- | Optionally parse an object, wrapping the result with Maybe.
maybeParse :: P s a -> P s (Maybe a)
maybeParse p = p ## Just // unit Nothing

measure :: P s Marker
measure = token "|" Measure

relative :: P s Marker
relative = token "\\relative" Relative

partial :: P s Marker
partial = token "\\partial" Partial

tie :: P s Marker
tie = token "~" Tie

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
                  , token "es" Es
                  , token "e" E
                  , token "f" F
                  , token "g" G
                  , token "a" A
                  , token "b" B
                  , token "r" R ]

pitch :: P s Pitch
pitch = diatonic <> accidentals <> octaves ## (uncurry . uncurry) PitchData

pitchMap :: M.Map Diatonic Int
pitchMap = M.fromList [ (C, 48)
                      , (D, 50)
                      , (Es, 51)
                      , (E, 52)
                      , (F, 53)
                      , (G, 55)
                      , (A, 57)
                      , (B, 59) ]

-- | Finalize pitches if they are rests.
doRests :: Pitch -> Pitch
doRests (PitchData R _ _) = Rest
doRests x = x

-- | Finalize an absolute pitch.
doAbsolute :: Pitch -> Pitch
doAbsolute (PitchData d as os) =
    let p = case M.lookup d pitchMap of
            Just x -> x
            Nothing -> error "Missing diatonic pitch"
        acc' x a = case a of
            Sharp -> x + 1
            Flat -> x - 1
        oct' x o = case o of
            OctaveUp -> x + 12
            OctaveDown -> x - 12
        p' = foldl acc' p as
        p'' = foldl oct' p' os
    in MIDIPitch p''
doAbsolute x = x

note :: P s Note
note = pitch <> maybeParse (duration ## undot) ## uncurry Note

chord :: P s Chord
chord = between (token "<" ()) (token ">" ()) (many note)
