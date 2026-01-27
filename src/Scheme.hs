{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Scheme
  ( HasValency(..)
  , valencyIs
  , Scheme(..)
  ) where

import           Data.Kind
import           Data.Proxy
import           Data.Text  (Text)

import           Resolve
import           Stream
import           Text

-- | Valency represents the maxiumum number of arguments a parser
-- might consume.
--
-- If the valency of a parser is 'Just n', then it consumes up to 'n'
-- arguments. If the valency is 'Nothing', it can consume an arbitrary
-- number of arguments.
class HasValency p where
  valency :: p r -> Maybe Integer

-- | Utility function to check how many arguments something supports.
valencyIs :: HasValency p => (Integer -> Bool) -> p r -> Bool
valencyIs condition = all condition . valency

-- | A type class for meant to parameterize 'ParseTree's.
-- Implementations are collections of parsers that can consume input
-- tokens and produce a result or throw an error.
class (Functor s, Resolve s) => Scheme (s :: Type -> Type) where
  -- | The token type this parser operates upon.
  data Token s

  -- | A 'Scheme' instance must provide a rendering function so that
  -- tokens can be displayed in error messages.
  renderToken :: Token s -> Builder

  -- | A 'Scheme' instance needs to provide a function to parse its
  -- tokens from 'Text' inputs. We parse from '[Text]' to '[Token s]'
  -- since it is possible that a single 'Text' might yield multiple
  -- tokens (or none).
  parseTokens :: [Text] -> [Token s]

  sepProd :: Proxy s -> Builder
  sepSum :: Proxy s -> Builder

  -- | Generate usage information for a 'Scheme' instance.
  renderParser :: s r -> Builder

  -- | Attempt activation of a provided parser with a given input
  -- stream. The parser either succeeds, fails, or does not apply.
  --
  -- Another way to interpret this operation is that it lifts a parser
  -- scheme into a 'StreamParser'.
  activate :: s r -> StreamParser (Token s) r

instance Scheme s => Render (Token s) where
  render = renderToken
