module Text.Lye.Interactive where

import Data.Maybe
import Text.Trifecta.Parser

import Text.Lye.Parser
import Text.Lye.Types

exprsFromFile :: FilePath -> IO [Expression]
exprsFromFile s = do
    mexprs <- parseFromFile fullParse s
    return $ fromMaybe [] mexprs
