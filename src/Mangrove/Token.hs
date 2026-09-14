{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-|
Module      : Mangrove.Token
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Parsing schemes convert arguments into some form of token. This module
defines the requirements for a scheme's associated token type.
-}

module Mangrove.Token
  ( HasTokens(..)
  ) where

import           Data.Kind
import           Data.Proxy

import           Mangrove.Render

-- | Parsing schemes convert arguments into some form of token. This
-- class defines the associated token type for a particular scheme.
class (Eq (Token s), Show (Token s), Render (Token s)) => HasTokens (s :: Type -> Type) where
  -- | A token represents a particular interpretation of an argument
  -- string.
  data Token s

  -- | 'delimiter' is the character that separates argument strings in
  -- combined string representation. For example, arguments in the CLI
  -- command @ls -a -l /var@ are separated by spaces.
  delimiter :: Proxy s -> Char
