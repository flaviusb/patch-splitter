{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Lib
    ( someFunc, patches, patch, Patch(..)
    ) where

import Text.Peggy
import Data.Text (Text, pack)
import Data.Maybe (catMaybes)
import Parser

someFunc :: IO ()
someFunc = putStrLn "someFunc"


