{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Lib
    ( someFunc
    ) where

import Text.Peggy

someFunc :: IO ()
someFunc = putStrLn "someFunc"
