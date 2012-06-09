-- | Gets command-line options
module Parser.Man.Options
( getOptions
, optDescr
, Options(..)
) where

import System.Console.GetOpt
import System.Environment
import System.IO

data Flag = Complete String | Help

-- | Command-line options
data Options = Options {
  completionFile :: String, -- ^ Where to write the completion file, or '-' for stdout
  help :: Bool
} deriving (Show)

defaultOptions = Options {
  completionFile = "-",
  help = False
}

modifyOption :: Flag -> Options -> Options
modifyOption Help opts = opts {help = True}
modifyOption (Complete s) opts = opts {completionFile = s}

-- | Gets command-line options
getOptions :: IO Options
getOptions = do
  args <- getArgs
  let (flags, _, _) = getOpt RequireOrder optDescr args
  return (foldr modifyOption defaultOptions flags)

-- | Description of command-line options for this tool
optDescr :: [OptDescr Flag]
optDescr =
  [ Option ['f'] ["file"] (ReqArg Complete "FILE") "Destination file for bash completions"
  , Option ['h', '?'] ["help"] (NoArg Help) "Print this help"
  ]

