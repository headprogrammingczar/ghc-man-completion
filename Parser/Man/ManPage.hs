{-# LANGUAGE FlexibleContexts #-}

-- | Parses the GHC man page
module Parser.Man.ManPage
( parseMan
)where

import Text.Parsec
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List

-- | The meat of the parser - takes the GHC man page contents and parses a list of parameters
parseMan :: (Stream s m Char) => ParsecT s u m [String]
parseMan = do
  manyTill anyChar (try (header "options"))
  results <- parseParams
  many anyChar >> eof
  return (nub results)

parseParams :: (Stream s m Char) => ParsecT s u m [String]
parseParams = do
  results <- many1 $ do
    spaces
    try parseOption <|> (manyTill anyChar (try space) >> return Nothing)
  return (catMaybes results)

parseOption :: (Stream s m Char) => ParsecT s u m (Maybe String)
parseOption = do
    char '-'
    name <- many1 (try alphaNum <|> try (char '-') <|> try (char '?'))
    many1 space
    return $ Just ('-':name)

anyHeader :: (Stream s m Char) => ParsecT s u m ()
anyHeader = do
  newline
  many1 (upper <|> space)
  newline
  return ()

header str = do
  newline
  string (map toUpper str)
  newline
  return ()

