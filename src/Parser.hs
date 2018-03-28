{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module Parser
     where

import Text.Peggy
import Data.Text (Text, pack, unlines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable

data Patch = Patch CommitLine CommitMetadata Diff deriving (Show, Data, Eq, Typeable)

data Diff = Diff [Change] deriving (Show, Data, Eq, Typeable)

data Change = Change ChunkHeader [ChangeOp] deriving (Show, Data, Eq, Typeable) {- oldpath newpath change_operation -}

data ChangeOp = ChangeOp Pos Lines deriving (Show, Data, Eq, Typeable)

data Pos = Pos Int Int Int Int deriving (Show, Data, Eq, Typeable)

data Lines = Lines [Line] Bool deriving (Show, Data, Eq, Typeable)

data Line = AddLine Text | RemoveLine Text | UnchangedLine Text deriving (Show, Data, Eq, Typeable)

data CommitLine = CommitLine Text [Text] (Maybe Text) deriving (Show, Data, Eq, Typeable) {- This hash, parent hashes, from hash -}

data CommitMetadata = CommitMetadata Text Text Text Text Text deriving (Show, Data, Eq, Typeable) {- Commit comment, author, author date, commit, commit date -}

data ChunkHeader = ChunkHeader (Maybe Text) Text deriving (Show, Data, Eq, Typeable)

[peggy|

nl :: String = [\r]? [\n] { "\n" }

liness :: Lines
  = ([-+ ] ([^\n\r]*) nl {
      case [$1] of
        " " -> UnchangedLine (pack $2)
        "+" -> AddLine (pack $2)
        "-" -> RemoveLine (pack $2)
    })+ ("\\ No newline at end of file" nl)? { Lines $1 ($2 == Nothing) }

hash :: Text
  = [a-f0-9]+ { pack $1 }

aftertext  :: Text
  = [^\n\r]+ { pack $1 }

commitline :: CommitLine
  = "commit " hash (" "+ hash { $2 })* aftertext?  { CommitLine $1 $2 $3 }

indentedtext :: Text
  = ([ ] [ ] [ ] [ ] aftertext nl { $5 })+ { Data.Text.unlines $1 }

metadata :: CommitMetadata
  = "Author:     " aftertext nl "AuthorDate: " aftertext nl "Commit:     " aftertext nl "CommitDate: " aftertext nl nl indentedtext { CommitMetadata $10 $1 $3 $5 $7 }

chunkheader :: ChunkHeader
  = "diff --git a/" [^ \n\r]+ [ ] "b/" [^ \n\r]+ nl ("new file mode " [^ \n\r]+ nl { "New File" })? "index " [^ \n\r]+ ([ ] [^ \n\r]+ { $2 })* nl "--- " (('/dev/null' { "/dev/null" }) / ("a/" [^ \n\r]+)) nl "+++ b/" [^ \n\r]+ { 
        case $5 of
          Nothing -> (ChunkHeader (Just $ pack $1) (pack $3))
          _       -> (ChunkHeader Nothing (pack $3))
    }

pos :: Pos
  = "@@ -" [0-9]+ [,] [0-9]+ [ ] [+] [0-9]+ [,] [0-9]+ [ ] '@@' { Pos (read $1) (read $3) (read $6) (read $8) }

changeop :: ChangeOp
  = pos nl? liness? { ChangeOp $1 $ fromMaybe (Lines [] True) $3 }

change :: Change
  = chunkheader nl (changeop { $1 }) { Change $1 [$3] }

diff :: Diff
  = (change { $1 })+ { Diff $1 }

patch :: Patch
  = commitline nl metadata nl diff { Patch $1 $3 $5 }

patches :: [Patch]
  = patch (nl patch { $2 })* { $1:$2 }

|]
