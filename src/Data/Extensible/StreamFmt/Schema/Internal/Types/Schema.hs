{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Extensible.StreamFmt.Schema.Internal.Types.Schema
  ( Schema
  , schema
  , schemaR
  , schemaOf
  , schemaOfR
  ) where

import           Control.Monad.Reader
import           Data.Extensible.StreamFmt.Schema.Internal.Types.IndentState
import           Data.Text.Lazy.Builder                                      (Builder)
import qualified Data.Text.Lazy.Builder                                      as B
import           Data.Typeable

class Schema a where
  schema :: Builder
  schemaOf :: Proxy a -> Builder
  schemaR :: Reader IndentState Builder
  schemaOfR :: Proxy a -> Reader IndentState Builder
  default schemaR :: (Typeable a) =>
    Reader IndentState Builder
  schemaR = pure $ B.fromString $ show $ typeRep (Proxy @a)
  schemaOfR _ = schemaR @a
  schema = runReader (schemaR @a) (zeroIndentState 2)
  schemaOf _ = schema @a
