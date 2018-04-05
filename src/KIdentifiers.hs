{-# Language FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings #-}

module KIdentifiers
    where

import Data.Text (Text, append)
import Control.Monad.Supply

data KVariable = KVariable Text
data KClass    = KClass Text

identlify :: (Text -> a) -> Text -> (Text -> a)
identlify typeconstructor prefix = \a -> typeconstructor $ append prefix a

variablify = identlify KVariable

new_commit = variablify "commit_"

new_class = KClass "Klass_"


