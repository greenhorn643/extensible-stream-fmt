{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Extensible.StreamFmt.Schema.Internal.Primitives
  ( specs
  ) where

import           Data.Extensible.StreamFmt.Schema.Internal.Primitives
import           Data.Extensible.StreamFmt.Schema.Internal.Types.Schema
import           Test.Hspec

import qualified Data.ByteString                               as B
import qualified Data.ByteString.Lazy                          as LB
import qualified Data.Text                                     as T
import qualified Data.Text.Lazy                                as LT

spec_PrimitivesToSchema :: Spec
spec_PrimitivesToSchema =
  describe "Schema describes primitives accurately" $ do
    it "describes Int" $
      schema @Int `shouldBe` "Int"
    it "describes Integer" $
      schema @Integer `shouldBe` "Integer"
    it "describes Float" $
      schema @Float `shouldBe` "Float"
    it "describes Double" $
      schema @Double `shouldBe` "Double"
    it "describes Text as String" $
      schema @T.Text `shouldBe` "String"
    it "describes Lazy Text as String" $
      schema @LT.Text `shouldBe` "String"
    it "describes ByteString as ByteString" $
      schema @B.ByteString `shouldBe` "ByteString"
    it "describes Lazy ByteString as ByteString" $
      schema @LB.ByteString `shouldBe` "ByteString"
    it "describes Bool" $
      schema @Bool `shouldBe` "Bool"
    it "describes Char" $
      schema @Char `shouldBe` "Char"
    it "describes ()" $
      schema @() `shouldBe` "()"
    it "describes String" $
      schema @String `shouldBe` "String"

spec_TuplesOfPrimitivesToSchema :: Spec
spec_TuplesOfPrimitivesToSchema =
  describe "Schema describes tuples of primitives accurately" $ do
    it "describes (Int, Double)" $
      schema @(Int, Double) `shouldBe` "(Int, Double)"
    it "describes (Text, (), Bool) as (String, (), Bool)" $
      schema @(T.Text, (), Bool) `shouldBe` "(String, (), Bool)"
    it "describes (Lazy ByteString, Float, Char, String) as (ByteString, Float, Char, String)" $
        schema @(LB.ByteString, Float, Char, String) `shouldBe` "(ByteString, Float, Char, String)"
    it "describes (Bool, Lazy Text, Integer, ByteString, String) as (Bool, String, Integer, ByteString, String)" $
      schema @(Bool, LT.Text, Integer, B.ByteString, String) `shouldBe` "(Bool, String, Integer, ByteString, String)"

spec_ListsOfPrimitivesToSchema :: Spec
spec_ListsOfPrimitivesToSchema =
  describe "Schema describes lists of primitives accurately" $ do
    it "describes [Int]" $
      schema @[Int] `shouldBe` "[Int]"
    it "describes [Integer]" $
      schema @[Integer] `shouldBe` "[Integer]"
    it "describes [Float]" $
      schema @[Float] `shouldBe` "[Float]"
    it "describes [Double]" $
      schema @[Double] `shouldBe` "[Double]"
    it "describes [Text] as [String]" $
      schema @[T.Text] `shouldBe` "[String]"
    it "describes [Lazy Text] as [String]" $
      schema @[LT.Text] `shouldBe` "[String]"
    it "describes [ByteString] as [ByteString]" $
      schema @[B.ByteString] `shouldBe` "[ByteString]"
    it "describes [Lazy ByteString] as [ByteString]" $
      schema @[LB.ByteString] `shouldBe` "[ByteString]"
    it "describes [Bool]" $
      schema @[Bool] `shouldBe` "[Bool]"
    it "describes [Char] as String" $
      schema @[Char] `shouldBe` "String"
    it "describes [()]" $
      schema @[()] `shouldBe` "[()]"
    it "describes [String]" $
      schema @[String] `shouldBe` "[String]"

spec_ListOfTuplesToSchema :: Spec
spec_ListOfTuplesToSchema =
  describe "Schema describes lists of tuples accurately" $ do
    it "describes [(Int, Text)] as [(Int, String)]" $
      schema @[(Int, T.Text)] `shouldBe` "[(Int, String)]"
    it "describes [(Bool, Char, [(Int, [Lazy ByteString])])] as [(Bool, Char, [(Int, [ByteString])])]" $
      schema @[(Bool, Char, [(Int, [LB.ByteString])])] `shouldBe` "[(Bool, Char, [(Int, [ByteString])])]"

spec_MaybeToSchema :: Spec
spec_MaybeToSchema =
  describe "Schema describes Schema a => Maybe a accurately" $ do
    it "describes Maybe Int" $
      schema @(Maybe Int) `shouldBe` "Maybe Int"
    it "describes Maybe Integer" $
      schema @(Maybe Integer) `shouldBe` "Maybe Integer"
    it "describes Maybe (Maybe Text) as Maybe (Maybe String)" $
      schema @(Maybe (Maybe T.Text)) `shouldBe` "Maybe (Maybe String)"
    it "describes Maybe (Maybe (Maybe Char))" $
      schema @(Maybe (Maybe (Maybe Char))) `shouldBe` "Maybe (Maybe (Maybe Char))"
    it "describes Maybe [Lazy ByteString] as Maybe [ByteString]" $
      schema @(Maybe [LB.ByteString]) `shouldBe` "Maybe [ByteString]"

spec_EitherToSchema :: Spec
spec_EitherToSchema =
  describe "Schema describes Schema a => Maybe a accurately" $ do
    it "describes Either Int String" $
      schema @(Either Int String) `shouldBe` "Either Int String"
    it "describes Either (Maybe (Either Text (Maybe Bool))) Lazy ByteString as Either (Maybe (Either String (Maybe Bool))) ByteString" $
      schema @(Either (Maybe (Either T.Text (Maybe Bool))) LB.ByteString) `shouldBe` "Either (Maybe (Either String (Maybe Bool))) ByteString"

specs :: Spec
specs =
  describe "Primitives" $ do
    spec_PrimitivesToSchema
    spec_TuplesOfPrimitivesToSchema
    spec_ListsOfPrimitivesToSchema
    spec_ListOfTuplesToSchema
    spec_MaybeToSchema
    spec_EitherToSchema