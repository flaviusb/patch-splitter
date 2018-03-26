{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Lib
    ( someFunc
    ) where

import Text.Peggy
import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Patch = Patch Text Text Text Diff  {- parents metadata hash? diffs -}

data Diff = Diff [Change]

data Change = Change Text Text ChangeOp {- oldpath newpath change_operation -}

data ChangeOp = ChangeOp Pos Pos [Line]

data Pos = Pos Int Int

data Line = AddLine Text | RemoveLine Text

[peggy|

nl :: String = [\n] [\r] { "\n\r" }

addline :: Line
  = " +" ([^\n\r]*) { AddLine    (pack $1) }

removeline :: Line
  = " -" ([^\n\r]*) { RemoveLine (pack $1) }

|]
