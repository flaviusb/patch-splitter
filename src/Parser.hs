import Data.Data
import Data.Typeable
data Patch = Patch CommitLine CommitMetadata Diff deriving (Show, Data, Eq, Typeable)
data Diff = Diff [Change] deriving (Show, Data, Eq, Typeable)
data Change = Change ChunkHeader ChangeOp deriving (Show, Data, Eq, Typeable) {- oldpath newpath change_operation -}
data ChangeOp = ChangeOp Pos Lines deriving (Show, Data, Eq, Typeable)
data Pos = Pos Int Int Int Int deriving (Show, Data, Eq, Typeable)
data Lines = Lines [Line] Bool deriving (Show, Data, Eq, Typeable)
data Line = AddLine Text | RemoveLine Text | UnchangedLine Text deriving (Show, Data, Eq, Typeable)
data CommitLine = CommitLine Text [Text] (Maybe Text) deriving (Show, Data, Eq, Typeable) {- This hash, parent hashes, from hash -}
data CommitMetadata = CommitMetadata Text Text Text Text Text deriving (Show, Data, Eq, Typeable) {- Commit comment, author, author date, commit, commit date -}

data ChunkHeader = ChunkHeader (Maybe Text) Text deriving (Show, Data, Eq, Typeable)
liness :: Lines
  = (addline / removeline / unchangedline)+ ("\ No newline at end of file" nl)? { Lines (catMaybes $1) ($2 == Nothing) }
  = "Author:     " aftertext nl "AuthorDate: " aftertext nl "Commit:     " aftertext nl "CommitDate: " aftertext nl nl indentedtext { CommitMetadata $10 $1 $3 $5 $7 }
  = "diff --git a/" [^ \n\r]+ " b/" [^ \n\r]+ nl ("new file mode " [^ \n\r]+ nl { "New File" })? "index " [^ \n\r]+ (" " [^ \n\r]+)? nl (("--- a/" [^ \n\r]+) / "--- /dev/null" { "/dev/null" }) nl "+++ b/" [^ \n\r]+ { 
  = "@@ -" [0-9]+ "," [0-9]+ " +" [0-9]+ "," [0-9]+ " @@" { Pos (read $1) (read $2) (read $3) (read $4) }

changeop :: ChangeOp
  = pos nl liness { ChangeOp $1 $3 }

change :: Change
  = chunkheader nl changeop { Change $1 $3 }

diff :: Diff
  = (change nl { $1 })+ { Diff $1 }

patch :: Patch
  = commitline nl metadata nl nl diff { Patch $1 $3 $6 }

patches :: [Patch]
  = (patch nl nl { $1 })+
