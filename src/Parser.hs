{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Parser
     where

import Text.Peggy
import Data.Text (Text, pack, unlines)
import Data.Maybe (catMaybes)

data Patch = Patch Text Text Text Diff  {- parents metadata hash? diffs -}

data Diff = Diff [Change]

data Change = Change Text Text ChangeOp {- oldpath newpath change_operation -}

data ChangeOp = ChangeOp Pos Pos [Line]

data Pos = Pos Int Int

data Line = AddLine Text | RemoveLine Text | UnchangedLine Text

data CommitLine = CommitLine Text [Text] (Maybe Text) {- This hash, parent hashes, from hash -}

data CommitMetadata = CommitMetadata Text Text Text Text Text {- Commit comment, author, author date, commit, commit date -}

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

hash :: Text
  = [a-f0-9]+ { pack $1 }

aftertext  :: Text
  = [^\n\r]+ { pack $1 }

commitline :: CommitLine
  = "commit " hash (" "+ hash { $2 })* aftertext?  { CommitLine $1 $2 $3 }

indentedtext :: Text
  = ("    " aftertext nl { $1 })+ { Data.Text.unlines $1 }

metadata :: CommitMetadata
  = "Author:     " aftertext nl "AuthorDate: " aftertext nl "Commit:     " aftertext nl "CommitDate: " aftertext nl indentedtext { CommitMetadata $9 $1 $3 $5 $7 }

|]
