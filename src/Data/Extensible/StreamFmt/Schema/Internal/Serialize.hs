{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Extensible.StreamFmt.Schema.Internal.Serialize
  (
  ) where

import           Control.Lens    hiding ((:>))
import           Data.Extensible
import           Data.Serialize
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as LT
import           Data.VLE
import           Data.Word

class (Serialize (TargetOf kv), kv ∈ kvs) =>
      MemberAndSerializeV kvs kv


instance (Serialize (TargetOf kv), kv ∈ kvs) => MemberAndSerializeV kvs kv

putRecord ::
     forall xs. Forall (MemberAndSerializeV xs) xs
  => Putter (Record xs)
putRecord =
  hfoldMapFor (Proxy @(MemberAndSerializeV xs)) (\(Field (Identity v)) -> put v)

getRecord ::
     forall xs. Forall (MemberAndSerializeV xs) xs
  => Get (Record xs)
getRecord =
  hgenerateFor (Proxy @(MemberAndSerializeV xs)) $ const (Field . pure <$> get)

instance Forall (MemberAndSerializeV xs) xs => Serialize (Record xs) where
  put = putRecord
  get = getRecord

putVariant ::
     forall xs. Forall (MemberAndSerializeV xs) xs
  => Putter (Variant xs)
putVariant v = do
  variableLengthEncode $ fromIntegral $ variantIndex v
  matchField (htabulateFor c $ \m -> Field (Match $ put . runIdentity)) v
  where
    c = Proxy @(MemberAndSerializeV xs)

getVariant :: Forall (MemberAndSerializeV xs) xs => Get (Variant xs)
getVariant = do
  i <- fromIntegral <$> variableLengthDecode
  fromIndexGetV i

instance Forall (MemberAndSerializeV xs) xs => Serialize (Variant xs) where
  get = getVariant
  put = putVariant

instance Serialize T.Text where
  put = put . T.unpack
  get = T.pack <$> get

instance Serialize LT.Text where
  put = put . LT.unpack
  get = LT.pack <$> get

variantIndex :: Variant xs -> Int
variantIndex = fromEnum . hoist fieldToProxy

fieldToProxy :: forall x. Field Identity x -> Proxy x
fieldToProxy _ = Proxy @x

proxyToGetField ::
     forall kv. Serialize (TargetOf kv)
  => Proxy kv
  -> Get (Field Identity kv)
proxyToGetField _ = Field . Identity <$> get

injectGetV ::
     forall kvs kv. MemberAndSerializeV kvs kv
  => Membership kvs kv
  -> Proxy kv
  -> Get (Variant kvs)
injectGetV _ p = do
  f <- proxyToGetField p
  return $ embed f

fromIndexGetV ::
     forall kvs. Forall (MemberAndSerializeV kvs) kvs
  => Int
  -> Get (Variant kvs)
fromIndexGetV i = match ms p
  where
    ms = htabulateFor (Proxy @(MemberAndSerializeV kvs)) (Match . injectGetV)
    p :: kvs :/ Proxy
    p = toEnum i
