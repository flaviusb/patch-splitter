{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Lib
import Options.Applicative as OA
import Data.Monoid((<>))
import Text.Peggy
import Control.Applicative
import System.IO
import Data.List
import Data.Text

data AppOptions = AppOptions {
  fileName :: String,
  outputFile :: String
}

appoptions :: OA.Parser AppOptions
appoptions = AppOptions
        <$> argument str
              ( metavar "FILE"
             <> help "File to parse" )
        <*> argument str
              ( metavar "FILE"
             <> help "Output file name")

real_main :: AppOptions -> IO ()
real_main options =
    do
      commit_text <- readFile $ fileName options
      let commits = parseString patches (fileName options) commit_text
          kotlin_code = case commits of
            Right commit_data -> completeKotlin commit_data
            Left  error       -> undefined
      putStrLn $ unpack kotlin_code

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Generate code from git log"
     <> header "patch-splitter - For testing CloudEFS against git repositories" )

