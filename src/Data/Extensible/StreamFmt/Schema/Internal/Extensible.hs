{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Data.Extensible.StreamFmt.Schema.Internal.Extensible
  (
  ) where

import           Control.Monad.Reader
import           Data.Extensible
import           Data.Extensible.StreamFmt.Schema.Internal.Types.IndentState
import           Data.Extensible.StreamFmt.Schema.Internal.Types.Schema
import qualified Data.Text.Lazy                                              as T
import           Data.Text.Lazy.Builder                                      (Builder)
import qualified Data.Text.Lazy.Builder                                      as B
import           GHC.TypeLits

instance Forall (KeyTargetAre KnownSymbol Schema) xs => Schema (Variant xs) where
  schemaR = indent $ do
    kvps <- schemaKVPs (Proxy @xs)
    nl <- newLine
    kvps' <- foldMapM
      (\((k, v), d) -> do
        nl <- newLine
        return $ nl <> d <> "\"" <> k <> "\"" <> " :> " <> v)
      (zip kvps ("[ " : repeat ", "))
    return $ "Variant" <>  kvps' <> nl <> "]"

instance Forall (KeyTargetAre KnownSymbol Schema) xs => Schema (Record xs) where
  schemaR = indent $ do
    kvps <- schemaKVPs (Proxy @xs)
    nl <- newLine
    kvps' <- foldMapM
      (\((k, v), d) -> do
        nl <- newLine
        return $ nl <> d <> "\"" <> k <> "\"" <> " :> " <> v)
      (zip kvps ("[ " : repeat ", "))
    return $ "Record" <>  kvps' <> nl <> "]"

foldMapM :: (Monoid m, Monad f) => (a -> f m) -> [a] -> f m
foldMapM f [] = pure mempty
foldMapM f (a:as) = do
  m <- f a
  m1 <- foldMapM f as
  return $ m <> m1

joinm :: Monoid m => m -> [m] -> m
joinm _ []       = mempty
joinm _ [m]      = m
joinm sep (m:ms) = m <> sep <> joinm sep ms

schemaKVPs ::
     forall xs. Forall (KeyTargetAre KnownSymbol Schema) xs
  => Proxy xs
  -> Reader IndentState [(Builder, Builder)]
schemaKVPs _ =
  henumerateFor
    (Proxy @(KeyTargetAre KnownSymbol Schema))
    (Proxy @xs)
    (\m kvps -> do
       sctail <- kvps
       scv <- schemaOfR (proxyTargetOf m)
       let sck = B.fromString $ symbolVal (proxyKeyOf m)
       return $ (sck, scv) : sctail)
    (pure mempty)
