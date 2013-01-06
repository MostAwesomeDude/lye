module Text.Lye.Export.File where

import qualified Codec.Midi as M
import Control.Lens
import Control.Monad.Free
import Data.List
import Data.Ord

import Text.Lye.Types

metaToTrack :: Meta a -> M.Track M.Ticks
metaToTrack (Pure _) = []
metaToTrack (Free (NoteOn t c p v f)) = (t, M.NoteOn c p v) : metaToTrack f
metaToTrack (Free (NoteOff t c p v f)) = (t, M.NoteOff c p v) : metaToTrack f

sortTrack :: M.Track M.Ticks -> M.Track M.Ticks
sortTrack = sortBy (comparing fst)

absToDelta :: Num a => [(a, b)] -> [(a, b)]
absToDelta track = let
    f current next = (next, next - current)
    (_, results) = mapAccumLOf (traverse . _1) f 0 track
    in results

export :: Int -> Meta a -> IO ()
export tpb meta = M.exportFile "test.mid" midi
    where
    midi = M.Midi M.SingleTrack (M.TicksPerBeat tpb) [track]
    track = absToDelta . sortTrack . metaToTrack $ meta
