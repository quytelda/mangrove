{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Mangrove.Scheme.Common
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module is for utilities used by more than one parsing scheme.
-}

module Mangrove.Scheme.Common
  ( keyEqualsValue
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
keyEqualsValue :: Text -> Maybe (Text, Text)
keyEqualsValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing
