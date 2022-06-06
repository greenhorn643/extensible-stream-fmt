{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Extensible.StreamFmt.Schema.Internal.Primitives
  (
  ) where

import           Data.Extensible.StreamFmt.Schema.Internal.Types.Schema

import qualified Data.ByteString                                             as B
import qualified Data.ByteString.Lazy                                        as LB
import           Data.Extensible.StreamFmt.Schema.Internal.Types.IndentState
import           Data.Kind
import qualified Data.Text                                                   as T
import qualified Data.Text.Lazy                                              as LT
import qualified Data.Text.Lazy.Builder                                      as B
import           Data.Typeable
import           GHC.TypeLits

instance Schema Int

instance Schema Integer

instance Schema Float

instance Schema Double

instance Schema T.Text where
  schemaR = pure "String"

instance Schema LT.Text where
  schemaR = pure "String"

instance Schema B.ByteString

instance Schema LB.ByteString

instance Schema Bool

instance Schema Char

instance Schema ()

instance (Schema a) => Schema [a] where
  schemaR = do
    sca <- schemaR @a
    let t = B.toLazyText sca
    if t == "Char" then pure "String"
                   else return $ "[" <> B.fromLazyText t <> "]"

instance (Schema a, Schema b) => Schema (a, b) where
  schemaR = do
    sca <- schemaR @a
    scb <- schemaR @b
    return $ "(" <> sca <> ", " <> scb <> ")"

instance (Schema a, Schema b, Schema c) => Schema (a, b, c) where
  schemaR = do
    sca <- schemaR @a
    scb <- schemaR @b
    scc <- schemaR @c
    return $ "(" <> sca <> ", " <> scb <> ", " <> scc <> ")"

instance (Schema a, Schema b, Schema c, Schema d) => Schema (a, b, c, d) where
  schemaR = do
    sca <- schemaR @a
    scb <- schemaR @b
    scc <- schemaR @c
    scd <- schemaR @d
    return $ "(" <> sca <> ", " <> scb <> ", " <> scc <> ", " <> scd <> ")"

instance (Schema a, Schema b, Schema c, Schema d, Schema e) =>
         Schema (a, b, c, d, e) where
  schemaR = do
    sca <- schemaR @a
    scb <- schemaR @b
    scc <- schemaR @c
    scd <- schemaR @d
    sce <- schemaR @e
    return $
      "(" <>
      sca <> ", " <> scb <> ", " <> scc <> ", " <> scd <> ", " <> sce <> ")"

instance (Schema a) => Schema (Maybe a) where
  schemaR = do
    sca <- requireGrouping $ schemaR @a
    groupIfRequired $ "Maybe " <> sca

instance (Schema a, Schema b) => Schema (Either a b) where
  schemaR = do
    sca <- requireGrouping $ schemaR @a
    scb <- requireGrouping $ schemaR @b
    groupIfRequired $ "Either " <> sca <> " " <> scb
