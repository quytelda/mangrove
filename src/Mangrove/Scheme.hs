{-# LANGUAGE TypeFamilies #-}

module Mangrove.Scheme
  ( Scheme(..)
  ) where

import           Data.Kind
import           Data.Proxy

import           Mangrove.Resolve
import           Mangrove.Stream
import           Mangrove.Text

-- | A scheme is a system of parsers and tokens. It parses a sequence
-- of arguments into tokens and values.
class Resolve s => Scheme (s :: Type -> Type) where
  -- | A token represents a particular interpretation of an argument
  -- string under this parsing scheme.
  data Token s

  -- | 'delimiter' is the character that separates argument strings in
  -- combined string representation. For example, arguments in the CLI
  -- command @ls -a -l /var@ are separated by spaces.
  delimiter :: Proxy s -> Char

  -- | Parse special control arguments that don't represent tokens in
  -- the scheme, but control aspects of how parsing proceeds (e.g.
  -- escaping).
  parseSpecials :: StreamParser (Token s) ()
  parseSpecials = pure ()

  -- | 'activate' tries to run a parser on the current input. If the
  -- parser doesn't apply, it consumes nothing and returns empty. If
  -- it does apply, it consumes the relevant input and returns a
  -- result.
  activate :: s r -> StreamParser (Token s) r

  -- | Render human-readable usage information for a particular
  -- parser.
  usageInfo :: s r -> Builder
