module Text.Lye.Interactive where

import Text.Lye.Parser
import Text.Lye.Types
import Text.Trifecta.Parser.ByteString

test :: String -> IO (Maybe Expression)
test = parseFromFile parseExpr
