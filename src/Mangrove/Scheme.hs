{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}

{-|
Module      : Mangrove.Scheme
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A "scheme" is a system of parsers and tokens. It determines the method
by which argument strings are separated. It parses a sequence of
arguments into tokens and values.
-}
module Mangrove.Scheme
  ( Scheme(..)
  , Modal(..)
  , usesTerseOutput
  , Exhibit(..)
  , exhibitToList
  ) where

import           Data.Kind
import           Data.Maybe
import           Data.Proxy

import           Mangrove.Resolve
import           Mangrove.Stream
import           Mangrove.Text

-- | A modal branch of a parser or parse tree is a particular subtree
-- that, when triggered, excludes the rest of the tree from receiving
-- input. This can happen because the parsing context changed (for
-- example, when a command is recognized) or when the parsing is
-- exited entirely (for example, when a help option is encountered).
data Modal a = Modal Bool a
  deriving (Functor)

instance Applicative Modal where
  pure = Modal False
  Modal terse1 f <*> Modal terse2 x = Modal (terse1 && terse2) (f x)

-- | Modal trees that exit the parsing flow entirely have no
-- opportunity to make use of optional parsers, since their values
-- will never be evaluated. Thus, we mark those trees as having
-- "terse" output so that we can omit the optional subtrees when
-- generating help information.
usesTerseOutput :: Modal a -> Bool
usesTerseOutput (Modal terseOutput _) = terseOutput

-- | An 'Exhibit' represents an object whose modal sub-components have
-- been split off for the purpose of better help output.
--
-- Every parser and parse tree can be decomposed into one regular tree
-- and a list of modal trees.
data Exhibit a = Exhibit (Maybe a) [Modal a]
  deriving (Functor)

-- | Convert an 'Exhibit' to a regular list of regular and modal
-- components.
exhibitToList :: Exhibit a -> [a]
exhibitToList (Exhibit mnorm modals) =
  maybeToList mnorm <> [t | Modal _ t <- modals]

-- | A scheme is a system of parsers and tokens. It parses a sequence
-- of arguments into tokens and values.
class (Functor s, Resolve s) => Scheme (s :: Type -> Type) where
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

  -- | Split out modal subtrees from a parser.
  exhibitParser :: s r -> Exhibit (s r)
  exhibitParser p = Exhibit (Just p) []

  -- | Render human-readable usage information for a particular
  -- parser.
  usageInfo :: s r -> Builder
