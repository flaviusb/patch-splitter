{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Lib
    ( someFunc, patches, patch, Patch(..)
    ) where

import Text.Peggy
import Data.Text (Text, pack)
import Data.Maybe (catMaybes)
import Parser
import GenerateCode

someFunc :: IO ()
someFunc = putStrLn "someFunc"


