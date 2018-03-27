{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Parser
     where

import Text.Peggy
import Data.Text (Text, pack)
import Data.Maybe (catMaybes)

data Patch = Patch Text Text Text Diff  {- parents metadata hash? diffs -}

data Diff = Diff [Change]

data Change = Change Text Text ChangeOp {- oldpath newpath change_operation -}

data ChangeOp = ChangeOp Pos Pos [Line]

data Pos = Pos Int Int

data Line = AddLine Text | RemoveLine Text | UnchangedLine Text

[peggy|

nl :: String = [\n] [\r] { "\n\r" }

addline :: Maybe Line
  = "+" ([^\n\r]*) nl { Just $ AddLine    (pack $1) }

removeline :: Maybe Line
  = "-" ([^\n\r]*) nl { Just $ RemoveLine (pack $1) }

unchangedline :: Maybe Line
  = " " ([^\n\r]*) nl { Just $ UnchangedLine (pack $1) }

lines :: [Line]
  = (addline / removeline / unchangedline)+ { catMaybes $1 }
|]
