import Prelude hiding ((.))

import Control.Category
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.Lens.Common
import qualified Data.Map as M
import Data.Maybe
import Data.STRef
import Debug.Trace
import Text.Parsers.Frisby
import Text.Parsers.Frisby.Char

import Text.Lye.Types

data Directive = Partial | Relative
    deriving (Eq, Show)

data Marker = Measure | Tie
    deriving (Eq, Show)

data Accidental = Flat | Sharp
    deriving (Eq, Show)

data Octave = OctaveUp | OctaveDown
    deriving (Eq, Show)

data Diatonic = C | D | Es | E | F | G | A | B | R
    deriving (Eq, Ord, Show)

data PitchData = PitchData Diatonic [Accidental] [Octave]
    deriving (Eq, Show)

data NoteData = NoteData PitchData (Maybe Duration)
    deriving (Eq, Show)

-- pitchNote = lens (\(Note p _) -> p) (\p (Note _ d) -> Note p d)
-- pitchDuration = lens (\(Note _ d) -> d) (\d (Note p _) -> Note p d)

type ChordData = [NoteData]

-- | Parse a string into an arbitrary object, consuming any leading
-- | whitespace.
token :: String -> a -> P s a
token s m = optional (many space) ->> (text s ##> m)

-- | Optionally parse an object, wrapping the result with Maybe.
maybeParse :: P s a -> P s (Maybe a)
maybeParse p = p ## Just // unit Nothing

relative :: P s Directive
relative = token "\\relative" Relative

partial :: P s Directive
partial = token "\\partial" Partial

measure :: P s Marker
measure = token "|" Measure

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
        modifySTRef dotted (`div` 2)
        dot <- readSTRef dotted
        modifySTRef duration (+ dot)
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

pitch :: P s PitchData
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

doAccidentals x a = case a of
    Sharp -> x + 1
    Flat -> x - 1
doOctaves x o = case o of
    OctaveUp -> x + 12
    OctaveDown -> x - 12

-- | Finalize an absolute pitch.
doAbsolute :: PitchData -> Pitch
doAbsolute (PitchData d as os) =
    let p = fromMaybe (error "Missing diatonic pitch") (M.lookup d pitchMap)
        p' = foldl doAccidentals p as
        p'' = foldl doOctaves p' os
    in p''

-- | Finalize a relative pitch.
-- | The previous pitch is curried in, to be used as a reference point; it
-- | must be a MIDIPitch, or else this function will just pass things through.
doRelative :: Pitch -> PitchData -> Pitch
doRelative prev (PitchData d as os) =
    let p = fromMaybe (error "Missing diatonic pitch") (M.lookup d pitchMap)
        -- Let's get in range.
        (to, tp) = prev `divMod` 12
        pp = p `mod` 12
        po = case compare pp tp of
            EQ -> to
            LT -> if tp - pp > 5 then to + 1 else to
            GT -> if pp - tp > 5 then to - 1 else to
        p' = po * 12 + pp
        p'' = foldl doAccidentals p' as
        p''' = foldl doOctaves p'' os
    in p'''

noteData :: P s NoteData
noteData = pitch <> maybeParse (duration ## undot) ## uncurry NoteData

chordData :: P s ChordData
chordData = between (token "<" ()) (token ">" ()) (many noteData)

data NoteMetadata = NM { nmDuration :: Int }

normalizeNotes :: (NoteMetadata, [Note]) -> [NoteData]
    -> (NoteMetadata, [Note])
normalizeNotes (nm, ns) [] = (nm, ns)
normalizeNotes (nm, ns) (NoteData p md:nds) =
    let d = fromMaybe (nmDuration nm) md
        n = Note (doAbsolute p) d
        nm' = nm { nmDuration = d }
    in normalizeNotes (nm', n:ns) nds

absoluteBlock :: P s [Note]
absoluteBlock = let initial = (NM { nmDuration = 120 }, [])
    in many noteData ## (\nds -> (reverse . snd) $ normalizeNotes initial nds)

-- carryDuration :: [Note] -> [Note]
-- carryDuration ns =
--    let loop :: Note -> State Duration Note
--        loop n = case getL pitchDuration n of
--            Just d -> put d >> return n
--            Nothing -> get >>= \d -> return $ setL pitchDuration (Just d) n
--    in (flip evalState) 120 $ forM ns loop

-- relativeBlock :: P s [Note]
-- relativeBlock =
--     let opener :: P s Pitch
--         opener = between relative (token "{" ()) pitch
--         loop :: Note -> State Pitch Note
--         loop n = do
--             p <- get
--             let p' = doRelative p $ getL pitchNote n
--             put p'
--             let n' = setL pitchNote p' n
--             return n'
--         zipper :: Pitch -> [Note] -> [Note]
--         zipper p ns = evalState (mapM loop ns) (doAbsolute p)
--         notes :: P s [Note]
--         notes = (opener <> many note) ## uncurry zipper
--     in notes <<- token "}" ()
