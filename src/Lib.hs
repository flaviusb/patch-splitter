{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Lib
    ( someFunc, patches, patch, Patch(..), completeKotlin
    ) where

import Text.Peggy
import Data.Text (Text, pack)
import Data.Maybe (catMaybes)
import Parser
import GenerateCode
import KotlinTemplates

someFunc :: IO ()
someFunc = putStrLn "someFunc"


