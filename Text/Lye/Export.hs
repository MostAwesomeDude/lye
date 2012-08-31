module Text.Lye.Export where

import Codec.Midi as M
import Control.Monad
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as St
import Data.Generics.Uniplate.Data

import Text.Lye.Types

type Exporter a = RWS Int (Track Ticks) Ticks a

note :: Channel -> Int -> Int -> M.Key -> Velocity -> Track Ticks
note chan start duration pitch vel =
    [ (start, NoteOn chan pitch vel)
    , (start + duration, NoteOff chan pitch vel) ]

scheduleNotes :: Expression -> Exporter Expression
scheduleNotes expr = do
    tpb <- ask
    position <- get
    -- Note that in all of our duration calculations, we are multiplying by 4
    -- as a constant. This is because our equation is:
    -- beats * ticks/beat * beats/measure
    -- The latter is a constant.
    case expr of
        SciNote pitch (Duration d) -> let
            duration' = floor $ d * (realToFrac tpb) * 4
            in do
                modify (+ duration')
                tell $ note 0 position duration' pitch 127
        Rest (Duration d) -> let
            duration' = floor $ d * (realToFrac tpb) * 4
            in modify (+ duration')
        _ -> return ()
    descendM scheduleNotes expr

fstMap :: (a -> b) -> [(a, c)] -> [(b, c)]
fstMap f xs = let inner (x, y) = (f x, y) in map inner xs

schedule :: Expression -> Int -> Track Ticks
schedule expr tpb = track
    where
    (_, _, track) = runRWS scheduler tpb 0
    scheduler = do
        -- tell $ [(0, TempoChange 500000)]
        _ <- scheduleNotes expr
        return ()

absToDelta :: Num a => [(a, b)] -> [(a, b)]
absToDelta track = let
    (as, ms) = unzip track
    as' = flip St.evalState 0 $ forM as $ \x' -> do
        x <- St.get
        St.put x'
        return $ x' - x
    in zip as' ms

export :: Int -> Track Ticks -> IO ()
export tpb track = exportFile "test.mid" $ Midi SingleTrack (TicksPerBeat tpb) [absToDelta track]
