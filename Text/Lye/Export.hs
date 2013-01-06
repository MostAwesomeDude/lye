module Text.Lye.Export where

import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import Text.Lye.Types

type Exporter = RWST Int () Int Meta

noteOn :: Int -> Int -> Int -> Int -> Meta ()
noteOn t c p v = liftF $ NoteOn t c p v ()

noteOff :: Int -> Int -> Int -> Int -> Meta ()
noteOff t c p v = liftF $ NoteOff t c p v ()

note :: Int -> Int -> Int -> Int -> Int -> Meta ()
note channel start duration pitch velocity = do
    noteOn start channel pitch velocity
    noteOff (start + duration) channel pitch velocity

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
                lift $ note 9 position duration' pitch 127
        SciNote pitch (Duration d) -> let
            duration' = doTPB d tpb
            in do
                modify (+ duration')
                lift $ note 0 position duration' pitch 127
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

schedule :: Expression -> Int -> Meta ()
schedule expr tpb = void $ runRWST (scheduleNotes expr) tpb 0
