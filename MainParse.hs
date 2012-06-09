-- | Parses the GHC man page and produces a bash completion file
module Main where

import Parser.Man.ManPage
import Parser.Man.Options

import Text.Parsec
import System.Console.GetOpt

import System.IO
import System.Process

main = do
  options <- getOptions
  case help options of
    True -> printHelp
    False -> do
      handle <- case completionFile options of
                  "-" -> return stdout
                  file -> openFile file WriteMode
      main' handle
      hClose handle

printHelp = do
  putStrLn (usageInfo "ghc-man-completion" optDescr)
  return ()

main' handle = do
  man <- readProcess "man" ["ghc"] ""
  man <- readProcess "perl" ["-pe", "s/\\e\\[?.*?[\\@-~]//g"] man
  case parse parseMan "man ghc" man of
    Left err -> print err
    Right opts -> hPutStrLn handle $ "complete -W \""++ unwords opts ++"\" ghc"
  return ()

