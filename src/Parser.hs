{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Parser
     where

import Text.Peggy
import Data.Text (Text, pack, unlines)
import Data.Maybe (catMaybes)

data Patch = Patch CommitLine CommitMetadata Diff

data Diff = Diff [Change]

data Change = Change ChunkHeader ChangeOp {- oldpath newpath change_operation -}

data ChangeOp = ChangeOp Pos Lines

data Pos = Pos Int Int Int Int

data Lines = Lines [Line] Bool

data Line = AddLine Text | RemoveLine Text | UnchangedLine Text

data CommitLine = CommitLine Text [Text] (Maybe Text) {- This hash, parent hashes, from hash -}

data CommitMetadata = CommitMetadata Text Text Text Text Text {- Commit comment, author, author date, commit, commit date -}

data ChunkHeader = ChunkHeader (Maybe Text) Text

[peggy|

nl :: String = [\n] [\r] { "\n\r" }

addline :: Maybe Line
  = "+" ([^\n\r]*) nl { Just $ AddLine    (pack $1) }

removeline :: Maybe Line
  = "-" ([^\n\r]*) nl { Just $ RemoveLine (pack $1) }

unchangedline :: Maybe Line
  = " " ([^\n\r]*) nl { Just $ UnchangedLine (pack $1) }

liness :: Lines
  = (addline / removeline / unchangedline)+ ("\ No newline at end of file" nl)? { Lines (catMaybes $1) ($2 == Nothing) }

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

chunkheader :: ChunkHeader
  = "diff --git a/" [^ \n\r]+ " b/" [^ \n\r]+ nl ("new file mode " [^ \n\r]+ nl { "New File" })? "index " [^ \n\r]+ " " [^ \n\r]+ nl "--- a/" [^ \n\r]+ nl "+++ b/" [^ \n\r]+ nl { 
        case $4 of
          Nothing -> (ChunkHeader Nothing (pack $2))
          _       -> (ChunkHeader (Just $ pack $1) (pack $2))
    }

pos :: Pos
  = "@@ -" [0-9]+ "," [0-9]+ " +" [0-9]+ "," [0-9]+ " @@" nl { Pos (read $1) (read $2) (read $3) (read $4) }

changeop :: ChangeOp
  = pos nl liness { ChangeOp $1 $3 }

change :: Change
  = chunkheader nl changeop { Change $1 $3 }

diff :: Diff
  = (change nl { $1 })+ { Diff $1 }

patch :: Patch
  = commitline nl metadata nl diff { Patch $1 $3 $5 }

|]
