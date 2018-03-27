data ChangeOp = ChangeOp Pos [Line]
data Pos = Pos Int Int Int Int
data ChunkHeader = ChunkHeader (Maybe Text) Text

chunkheader :: ChunkHeader
  = "diff --git a/" [^ \n\r]+ " b/" [^ \n\r]+ nl ("new file mode " [^ \n\r]+ nl { "New File" })? "index " [^ \n\r]+ " " [^ \n\r]+ nl "--- a/" [^ \n\r]+ nl "+++ b/" [^ \n\r]+ nl { 
        case $4 of
          Nothing -> (ChunkHeader Nothing (pack $2))
          _       -> (ChunkHeader (Just $ pack $1) (pack $2))
    }

pos :: Pos
  = "@@ -" [0-9]+ "," [0-9]+ " +" [0-9]+ "," [0-9]+ " @@" nl { Pos (read $1) (read $2) (read $3) (read $4) }