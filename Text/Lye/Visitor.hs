module Text.Lye.Visitor where

import Data.Ratio
import Text.Lye.Parser

traverseExpression ::
    (Expression -> (Expression, Bool)) -> Expression -> Expression
traverseExpression f expr =
    let (expr', recurse) = f expr
        expr'' = case expr' of
            Chord es -> Chord $ map (traverseExpression f) es
            Drums e -> Drums $ traverseExpression f e
            Music es -> Music $ map (traverseExpression f) es
            RawNote c a o me -> RawNote c a o $ fmap (traverseExpression f) me
            Relative c os e -> Relative c os $ traverseExpression f e
            Rest me -> Rest $ fmap (traverseExpression f) me
            Times fr e -> Times fr $ traverseExpression f e
            Voice es -> Voice $ map (traverseExpression f) es
            Voices es -> Voices $ map (traverseExpression f) es
            x -> x
    in if recurse then expr'' else expr'

transformDrums :: Expression -> (Expression, Bool)
transformDrums (Drums e) = (e, True)
transformDrums x = (x, True)

transformVoices :: Expression -> (Expression, Bool)
transformVoices (Voices es) = (Voices (map (\(Music x) -> Voice x) es), True)
transformVoices x = (x, True)

undotDuration :: Integer -> Integer -> Fraction
undotDuration ticks dots = 
    let duration = 120 * 4 % ticks
        denominator = 2 ^ dots
        numerator = denominator * 2 - 1
    in (numerator % denominator) * duration

simplify :: Expression -> Expression
simplify e =
    let e1 = traverseExpression transformDrums e
        e2 = traverseExpression transformVoices e
    in e2
