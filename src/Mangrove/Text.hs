{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Mangrove.Text
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Utilities for dealing with various types of text.
-}
module Mangrove.Text
  ( -- * Text Rendering
    Render(..)
  , renderLazyText
  , renderText
  , putBuilder
  , hPutBuilder

    -- * Helpers
  , keyEqualsValue

    -- * Re-exports
  , Builder
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TLIO
import           System.IO

-- | A class for things that can be rendered to a text 'Builder'.
class Render a where
  render :: a -> Builder

instance Render Builder where
  render = id

instance Render T.Text where
  render = TLB.fromText

instance Render Char where
  render = TLB.singleton

instance Render String where
  render = TLB.fromString

-- | Convert renderable data directly to lazy 'TL.Text'.
renderLazyText :: Render a => a -> TL.Text
renderLazyText = TLB.toLazyText . render

-- | Convert renderable data directly to strict 'T.Text'.
renderText :: Render a => a -> Text
renderText = TL.toStrict . TLB.toLazyText . render

-- | Write the contents of a 'Builder' to standard output.
putBuilder :: Builder -> IO ()
putBuilder = TLIO.putStr . TLB.toLazyText

-- | Write the contents of a 'Builder' to some IO handle.
hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder handle = TLIO.hPutStr handle . TLB.toLazyText

--------------------------------------------------------------------------------
-- Utility Functions

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
keyEqualsValue :: Text -> Maybe (Text, Text)
keyEqualsValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing
