{-# Language FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings #-}

module KIdentifiers
    where

import Data.Text (Text, append)
import Control.Monad.Supply

data KVariable = KVariable Text
data KClass    = KClass Text

identlify :: (Text -> a) -> Text -> Supply Text a
identlify typeconstructor prefix = do
    suffix <- supply
    return $ typeconstructor $ append prefix suffix

variablify = identlify KVariable

new_commit = variablify "commit_"

new_content_accumulator = variablify "content_"

new_class = KClass "Klass_"


