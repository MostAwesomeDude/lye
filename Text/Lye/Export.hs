module Text.Lye.Export where

import Codec.Midi
import Control.Monad.Trans.Reader

doTPB :: Time -> Int -> Ticks
doTPB x y = floor $ x / (realToFrac y)

note :: Channel -> Time -> Time -> Key -> Velocity -> Reader Ticks (Track Ticks)
note chan start duration pitch vel = do
    tpb <- ask
    let start' = doTPB start tpb
        duration' = doTPB duration tpb
    return [ (start', NoteOn chan pitch vel)
           , (start' + duration', NoteOff chan pitch vel) ]
