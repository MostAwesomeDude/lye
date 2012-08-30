module Text.Lye.Visitor where

import Debug.Trace

import Control.Monad.Trans.State
import Data.Functor
import Data.Ratio
import Text.Lye.Pitches
import Text.Lye.Types

import Data.Generics.Uniplate.Data

applyPeephole :: ([Expression] -> [Expression]) -> Expression -> Expression
applyPeephole f = let
    g (Chord es) = Chord $ f es
    g (Music es) = Music $ f es
    g (Voice es) = Voice $ f es
    g (Voices es) = Voices $ f es
    g e = e
    in transform g

inlineDrums :: Expression -> Expression
inlineDrums = let
    f (Drums expr) = Just expr
    f _ = Nothing
    in rewrite f

preserveVoices :: Expression -> Expression
preserveVoices = let
    f (Voices vs) = Just $ Voices [ Voice exprs | (Music exprs) <- vs ]
    f _ = Nothing
    in rewrite f

-- | Apply durations to an expression.
--   Uses the State monad to move data across the AST, and manually recurses
--   in the correct manner.
applyDurations :: Expression -> Expression
applyDurations expr = let
    f (ParsedNote c a o (Just d)) = put d >> return (Note c a o d)
    f (ParsedNote c a o Nothing) = Note c a o <$> get
    f (ParsedRest (Just d)) = put d >> return (Rest d)
    f (ParsedRest Nothing) = Rest <$> get
    f x = return x
    recurser e = f e >>= descendM recurser
    initial = Duration (1 % 4)
    in evalState (recurser expr) initial

applyTimes :: Expression -> Expression
applyTimes = let
    inner r (Duration r') = Duration $ r * r'
    f (Times r expr) = Just $ transformBi (inner r) expr
    f _ = Nothing
    in rewrite f

-- | Apply Relative to inner expressions.
relativize :: Expression -> Expression
relativize = rewrite f
    where
    f (Relative p os exprs) = Just $ evalState (recurser exprs) (p, os)
    f _ = Nothing
    g (Note p a o d) = do
        (rp, ro) <- get
        let whether = transposeDirection rp p
            o' = os . ocount $ o ++ ro ++ if whether then [OctaveDown] else []
        put (p, o')
        return $ Note p a o' d
    g x = return x
    ocount = sum . map (\x -> case x of
        OctaveUp   -> 1
        OctaveDown -> -1)
    os i = if i < 0
        then replicate (negate i) OctaveUp
        else replicate i OctaveDown
    recurser e = g e >>= descendM recurser

flattenMusic :: Expression -> Expression
flattenMusic = rewrite f
    where
    f (Music [x]) = Just x
    f (Music xs) | anyMusic xs = Just . Music $ refold xs
    f _ = Nothing
    anyMusic xs = not . null $ [x | (Music x) <- xs]
    refold [] = []
    refold ((Music exprs):xs) = exprs ++ refold xs
    refold (x:xs) = x : refold xs

dumpExpr :: Expression -> Expression
dumpExpr expr = trace ("Currently at " ++ show expr) expr

stages :: [Expression -> Expression]
stages = [ inlineDrums
         , preserveVoices
         , applyDurations
         , applyTimes
         , relativize
         , flattenMusic
         -- , NoteTransformer
         -- , DynamicRemover
         -- , ChordSorter
         -- , SlurMaker
         -- , TieRemover
         -- , RestMerger
         ]

applyStages :: Expression -> Expression
applyStages = flip (foldl $ flip ($)) stages

longestChord :: Expression -> Int
longestChord = let
    f (Chord xs) is = maximum $ length xs:is
    f _ is = maximum $ 0:is
    in para f
