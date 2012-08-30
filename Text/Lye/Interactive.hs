module Text.Lye.Interactive where

import Text.Lye.Parser
import Text.Lye.Types
import Text.Trifecta.Parser

exprsFromFile :: FilePath -> IO [Expression]
exprsFromFile s = do
    mexprs <- parseFromFile fullParse s
    return $ case mexprs of
        Just exprs -> exprs
        Nothing -> []
