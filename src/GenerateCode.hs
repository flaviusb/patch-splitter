{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module GenerateCode
     where

import Data.Text (Text, pack)
import Data.Text as DT (concat, lines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable
import Parser
import Data.List

applyChange :: Text -> Change -> Text
applyChange txt (Change _ ops) = foldl applyOp txt ops

applyOp :: Text -> ChangeOp -> Text
applyOp txt (ChangeOp (Pos startline startextent endline endextent) l)
    = DT.concat [DT.concat (fst a), processed, DT.concat (snd b)]
        where
          foo = (DT.lines txt)
          a = splitAt startline foo
          b = splitAt startextent (snd a)
          processed = process (fst b) l

process :: [Text] -> Lines -> Text
process txt changes = undefined
