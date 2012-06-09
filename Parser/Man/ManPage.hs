-- | Parses the GHC man page
module Parser.Man.ManPage where

import Text.Parsec
import Data.Char

-- | A list of GHC parameters and their documentation
data Result = Result [(String, String)]

type Parser = Parsec String ()

-- skip past the intro to OPTIONS
-- then skip OPTIONS, because it is just a summary
-- accumulate these sections until we get to FILES
parse :: Parser Result
parse = do
  manyTill anyChar (try (header "options"))
  manyTill anyChar (try anyHeader)
  -- PARSE!
  let emptyResults = Result []
  results <- many parseHeaders

  manyTill anyChar (try (header "files"))
  return (joinResults results)

joinResults :: [Result] -> Result
joinResults = foldr (\(Result a) (Result b) -> Result (a ++ b)) (Result [])

parseHeaders :: Parser Result
parseHeaders = do
  return (Result [])

anyHeader = newline >> many1 (upper <|> space) >> newline

header str = newline >> string (map toUpper str) >> newline

