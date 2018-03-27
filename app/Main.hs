module Main where

import Lib
import Options.Applicative
import Data.Monoid((<>))

data AppOptions = AppOptions {
  fileName :: String
}

appoptions :: Parser AppOptions
appoptions = AppOptions
        <$> argument str
              ( metavar "FILE"
             <> help "File to parse" )

real_main :: AppOptions -> IO ()
real_main options =
    do
      someFunc

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Generate code from git log"
     <> header "patch-splitter - For testing CloudEFS against git repositories" )

