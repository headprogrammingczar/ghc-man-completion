-- | Parses the GHC man page and produces a bash completion file
module Main where

import Text.Parsec
import Parser.Man.Options

main = do
  options <- getOptions
  print options

