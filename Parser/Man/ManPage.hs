-- | Parses the GHC man page
module Parser.Man.ManPage where

import Text.Parsec

-- | A list of GHC parameters and their documentation
data Result = Result [(String, String)]

parse :: Parser Result
parse = do
  return (Result [])

