module Text.Lye.Interactive where

import Text.Lye.Parser
import Text.Lye.Types
import Text.Trifecta.Parser.ByteString

test :: String -> IO [Expression]
test s = do
    mexprs <- parseFromFile parseExprs s
    return $ case mexprs of
        Just exprs -> exprs
        Nothing -> []
