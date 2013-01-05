module Text.Lye.Export where

import qualified Codec.Midi as M
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as St
import Data.List
import Data.Ord

import Text.Lye.Types

type Exporter a = RWS Int (M.Track M.Ticks) M.Ticks a

type Meta a = Free Notes a

metaToTrack :: Meta a -> M.Track M.Ticks
metaToTrack (Pure _) = []
metaToTrack (Free (NoteOn t c p v f)) = (t, M.NoteOn c p v) : metaToTrack f
metaToTrack (Free (NoteOff t c p v f)) = (t, M.NoteOff c p v) : metaToTrack f

note :: M.Channel -> Int -> Int -> M.Key -> M.Velocity -> M.Track M.Ticks
note chan start duration pitch vel =
    [ (start, M.NoteOn chan pitch vel)
    , (start + duration, M.NoteOff chan pitch vel) ]

-- Note that in all of our duration calculations, we are multiplying by 4 as a
-- constant. This is because our equation is:
-- beats * ticks/beat * beats/measure
-- The latter is a constant.
doTPB :: Rational -> Int -> Int
doTPB d tpb = floor $ d * realToFrac tpb * 4

scheduleNotes :: Expression -> Exporter Expression
scheduleNotes expr = do
    tpb <- ask
    position <- get
    case expr of
        DrumNote drum (Duration d) -> let
            duration' = doTPB d tpb
            pitch = fromEnum drum
            in do
                modify (+ duration')
                tell $ note 9 position duration' pitch 127
        SciNote pitch (Duration d) -> let
            duration' = doTPB d tpb
            in do
                modify (+ duration')
                tell $ note 0 position duration' pitch 127
        Rest (Duration d) -> let
            duration' = doTPB d tpb
            in modify (+ duration')
        Voices exprs -> forM_ exprs $ \e -> do
            put position
            scheduleNotes e
        _ -> return ()
    case expr of
        Voices _ -> return expr
        _ -> plate scheduleNotes expr

schedule :: Expression -> Int -> M.Track M.Ticks
schedule expr tpb = sortBy (comparing fst) track
    where
    (_, _, track) = runRWS scheduler tpb 0
    scheduler = void $ scheduleNotes expr

absToDelta :: Num a => [(a, b)] -> [(a, b)]
absToDelta track = let
    (as, ms) = unzip track
    as' = flip St.evalState 0 $ forM as $ \x' -> do
        x <- St.get
        St.put x'
        return $ x' - x
    in zip as' ms

export :: Int -> M.Track M.Ticks -> IO ()
export tpb track = M.exportFile "test.mid" midi
    where midi = M.Midi M.SingleTrack (M.TicksPerBeat tpb) [absToDelta track]
