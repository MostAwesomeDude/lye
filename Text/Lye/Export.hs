module Text.Lye.Export where

import Codec.Midi as M
import Data.Generics.Uniplate.Data

import Text.Lye.Types

-- Based on...
-- para :: Uniplate on => (on -> [r] -> r) -> on -> r
-- para op x = op x $ map (para op) $ children x
-- I *think* this is right. I think.
-- paraM :: (Monad m, Uniplate on) => (on -> [r] -> m r) -> on -> m r
-- paraM op x = join . (liftM $ op x) $ mapM (paraM op) $ children x

note :: Channel -> Int -> Int -> M.Key -> Velocity -> Track Int
note chan start duration pitch vel =
    [ (start, NoteOn chan pitch vel)
    , (start + duration, NoteOff chan pitch vel) ]

paramorph :: Expression -> [Track Int] -> Track Int
paramorph expr tracks = let
    track = case expr of
        SciNote pitch duration -> note 0 0 duration pitch 127
        _ -> []
    in concat $ track:tracks

doTPB :: Int -> Int -> Ticks
doTPB = flip div

schedule :: Expression -> Int -> Track Int
schedule expr tpb = para paramorph expr
