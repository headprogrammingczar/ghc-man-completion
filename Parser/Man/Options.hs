-- | Gets command-line options
module Parser.Man.Options
( getOptions
, Options(..)
) where

import System.Console.GetOpt
import System.Environment
import System.IO

data Flag = Complete String | Verbose deriving (Eq)

-- | Command-line options
data Options = Options {
  completionFile :: String, -- ^ Where to write the completion file, or '-' for stdout
  verbose :: Bool
} deriving (Show)

defaultOptions = Options {
  completionFile = "-",
  verbose = False
}

modifyOption :: Flag -> Options -> Options
modifyOption Verbose opts = opts {verbose = True}
modifyOption (Complete s) opts = opts {completionFile = s}

-- | Gets command-line options
getOptions :: IO Options
getOptions = do
  args <- getArgs
  let (flags, _, _) = getOpt RequireOrder optDescr args
  return (foldr modifyOption defaultOptions flags)

optDescr :: [OptDescr Flag]
optDescr =
  [ Option ['f'] ["file"] (ReqArg Complete "FILE") "Destination file for bash completions"
  , Option ['v'] ["verbose"] (NoArg Verbose) "Print debug output to stderr"
  ]

