{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings #-}

module GenerateCode
     where

import Data.Text (Text, pack)
import Data.Text as DT (concat, lines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable
import Parser
import Data.List
import Text.Shakespeare.Text
import Control.Monad.Supply
import KotlinTemplates
import KIdentifiers

applyChange :: Text -> Change -> Text
applyChange txt (Change _ ops) = foldl applyOp txt ops

applyOp :: Text -> ChangeOp -> Text
applyOp txt (ChangeOp (Pos startline startextent endline endextent) l)
    = DT.concat [DT.concat (fst a), processed, DT.concat (snd b)]
        where
          foo = (DT.lines txt)
          a = splitAt startline foo
          b = splitAt startextent (snd a)
          processed = process (fst b) l []

process :: [Text] -> Lines -> [Text] -> Text
process (t:txt) (Lines (change:rest) nl) acc = case change of
    AddLine line       -> process (t:txt) (Lines rest nl) (line:acc)
    RemoveLine line    -> if t == line then (process txt (Lines rest nl) (acc)) else undefined
    UnchangedLine line -> if t == line then (process txt (Lines rest nl) (line:acc)) else undefined

process [] (Lines (change:rest) nl) acc = case change of
    AddLine line       -> process [] (Lines rest nl) (line:acc)
    _                  -> undefined

process (t:txt) (Lines [] nl) acc = undefined

process [] (Lines [] nl) acc = DT.concat $ if nl then (acc ++ ["\n"]) else acc

{-
generateFiles :: 


section :: Diff -> [Text] -> Text

tinySection Change -> 

-}
