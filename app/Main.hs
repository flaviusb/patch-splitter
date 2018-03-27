{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Lib
import Options.Applicative as OA
import Data.Monoid((<>))
import Text.Peggy
import Control.Applicative
import System.IO
import Data.List

data AppOptions = AppOptions {
  fileName :: String
}

appoptions :: OA.Parser AppOptions
appoptions = AppOptions
        <$> argument str
              ( metavar "FILE"
             <> help "File to parse" )

real_main :: AppOptions -> IO ()
real_main options =
    do
      commit_text <- readFile $ fileName options
      let commits = parseString patches "placeholder" commit_text
      putStrLn $ show commits

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Generate code from git log"
     <> header "patch-splitter - For testing CloudEFS against git repositories" )

