module Text.Lye.Export where

import Codec.Midi as M
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Generics.Uniplate.Data

import Text.Lye.Types

-- Based on...
-- para :: Uniplate on => (on -> [r] -> r) -> on -> r
-- para op x = op x $ map (para op) $ children x
-- I *think* this is right. I think.
paraM :: (Monad m, Uniplate on) => (on -> [r] -> m r) -> on -> m r
paraM op x = join . (liftM $ op x) $ mapM (paraM op) $ children x

note :: Channel -> Int -> Int -> M.Key -> Velocity -> Track Ticks
note chan start duration pitch vel =
    [ (start, NoteOn chan pitch vel)
    , (start + duration, NoteOff chan pitch vel) ]

paramorph :: Expression -> [Track Int] -> State Exporter (Track Ticks)
paramorph expr tracks = do
    position <- use eTicks
    tpb <- use eTpb
    -- Note that in all of our duration calculations, we are multiplying by 4
    -- as a constant. This is because our equation is:
    -- beats * ticks/beat * beats/measure
    -- The latter is a constant.
    track <- case expr of
        SciNote pitch (Duration d) -> let
            duration' = floor $ d * (realToFrac tpb) * 4
            in do
                eTicks += duration'
                return $ note 0 position duration' pitch 127
        _ -> return []
    return . concat $ track:tracks

fstMap :: (a -> b) -> [(a, c)] -> [(b, c)]
fstMap f xs = let inner (x, y) = (f x, y) in map inner xs

schedule :: Expression -> Int -> Track Ticks
schedule expr tpb = evalState (paraM paramorph expr) $ Exporter tpb 0
