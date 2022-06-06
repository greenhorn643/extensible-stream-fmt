{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Test.Extensible.StreamFmt.Schema.Internal.Extensible where

import           Data.Extensible
import           Data.Extensible.StreamFmt.Schema.Internal.Extensible
import           Data.Extensible.StreamFmt.Schema.Internal.Primitives
import           Data.Extensible.StreamFmt.Schema.Internal.Types.Schema
import           Test.Hspec
import           Text.RawString.QQ

import           Data.ByteString                                        (ByteString)
import           Data.Text                                              (Text)

spec_RecordsToSchema :: Spec
spec_RecordsToSchema =
  describe "Schema describes extensible records accurately" $ do
    it [r|describes Record ["x" :> Double, "y" :> Double]|] $
      schema @(Record
        [ "x" :> Double
        , "y" :> Double
        ]) `shouldBe`[r|Record
  [ "x" :> Double
  , "y" :> Double
  ]|]
    it [r|describes Record
  [ "integer" :> Integer
  , "string"  :> String
  , "text"    :> Text
  , "tuple"   :> (Bool, ByteString)
  , "list"    :> [()]
  ]|] $
      schema @(Record
        [ "integer" :> Integer
        , "string"  :> String
        , "text"    :> Text
        , "tuple"   :> (Bool, ByteString)
        , "list"    :> [()]
        ]) `shouldBe` [r|Record
  [ "integer" :> Integer
  , "string" :> String
  , "text" :> String
  , "tuple" :> (Bool, ByteString)
  , "list" :> [()]
  ]|]

spec_VariantsToScehma :: Spec
spec_VariantsToScehma =
  describe "Schema describes extensible variants accurately" $ do
    it [r|describes Variant ["x" :> Double, "y" :> Double]|] $
      schema @(Variant
        [ "x" :> Double
        , "y" :> Double
        ]) `shouldBe`[r|Variant
  [ "x" :> Double
  , "y" :> Double
  ]|]
    it [r|describes
Variant
  [ "integer" :> Integer
  , "string"  :> String
  , "text"    :> Text
  , "tuple"   :> (Bool, ByteString)
  , "list"    :> [()]
  ]|] $
      schema @(Variant
        [ "integer" :> Integer
        , "string"  :> String
        , "text"    :> Text
        , "tuple"   :> (Bool, ByteString)
        , "list"    :> [()]
        ]) `shouldBe` [r|Variant
  [ "integer" :> Integer
  , "string" :> String
  , "text" :> String
  , "tuple" :> (Bool, ByteString)
  , "list" :> [()]
  ]|]

spec_NestedToSchema :: Spec
spec_NestedToSchema =
  describe "Schema describes nested extensible data structures accurately" $ do
    it [r|describes Record
  [ "x" :> Record
    [ "a" :> Int
    , "b" :> Char
    ]
  , "y" :> Variant
    [ "q" :> Double
    , "u" :> Float
    ]
  ]|] $
      schema @(Record
        [ "x" :> Record [ "a" :> Int, "b" :> Char ]
        , "y" :> Variant [ "q" :> Double, "u" :> Float ]
        ]) `shouldBe` [r|Record
  [ "x" :> Record
    [ "a" :> Int
    , "b" :> Char
    ]
  , "y" :> Variant
    [ "q" :> Double
    , "u" :> Float
    ]
  ]|]
    it [r|describes Variant
  [ "a0" :> Variant
    [ "a00" :> Variant
      [ "a000" :> Int
      , "a001" :> Float
      ]
    , "a01" :> Variant
      [ "a010" :> String
      , "a011" :> ByteString
      ]
    ]
  , "a1" :> Variant
    [ "a10" :> Char
    , "a11" :> Int
    ]
  ]|] $
      schema @(Variant
      [ "a0" :> Variant
        [ "a00" :> Variant
          [ "a000" :> Int
          , "a001" :> Float
          ]
        , "a01" :> Variant
          [ "a010" :> String
          , "a011" :> ByteString
          ]
        ]
      , "a1" :> Variant
        [ "a10" :> Char
        , "a11" :> Int
        ]
      ]) `shouldBe` [r|Variant
  [ "a0" :> Variant
    [ "a00" :> Variant
      [ "a000" :> Int
      , "a001" :> Float
      ]
    , "a01" :> Variant
      [ "a010" :> String
      , "a011" :> ByteString
      ]
    ]
  , "a1" :> Variant
    [ "a10" :> Char
    , "a11" :> Int
    ]
  ]|]

specs :: Spec
specs =
  describe "Extensible" $ do
    spec_RecordsToSchema
    spec_VariantsToScehma
    spec_NestedToSchema
