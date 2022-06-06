{-# LANGUAGE OverloadedStrings #-}

module Data.Extensible.StreamFmt.Schema.Internal.Types.IndentState
  ( IndentState
  , newLine
  , indent
  , groupIfRequired
  , requireGrouping
  , zeroIndentState
  ) where

import           Control.Monad.Reader
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

data IndentState =
  IndentState
    { indentLevel       :: Int
    , groupingRequired  :: Bool
    , spacesPerIndent   :: Int
    }

newLine :: Reader IndentState Builder
newLine = do
  il <- asks indentLevel
  return $ B.fromString $ '\n' : replicate il ' '

indent :: Reader IndentState a -> Reader IndentState a
indent = local $ \(IndentState il gr spi) -> IndentState (il + spi) gr spi

groupIfRequired :: Builder -> Reader IndentState Builder
groupIfRequired s = do
  gr <- asks groupingRequired
  return $ if gr
    then "(" <> s <> ")"
    else s

requireGrouping :: Reader IndentState a -> Reader IndentState a
requireGrouping = local $ \(IndentState il gr spi) -> IndentState il True spi

zeroIndentState :: Int -> IndentState
zeroIndentState = IndentState 0 False
