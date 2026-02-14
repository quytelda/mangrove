{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module ParseTree
  ( ParseTree(..)
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import qualified Data.List            as List
import           Data.Proxy
import           Data.Text            (Text)

import           Resolve
import           Result
import           Stream
import           Text

-- | 'ParseTree scheme r' is an expression tree composed of parsers
-- from scheme 'scheme' which evaluates to a value of type 'r' when
-- supplied with the proper input.
data ParseTree (scheme :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value (abstracts 'empty')
  EmptyNode :: ParseTree scheme r
  -- | A terminal node with a resolved value (abstracts 'pure')
  ValueNode :: r -> ParseTree scheme r
  -- | A parser awaiting input
  ParseNode :: scheme r -> ParseTree scheme r
  -- | Abstracts 'liftA2' and by extension '(<*>)'
  ProdNode :: (u -> v -> r) -> ParseTree scheme u -> ParseTree scheme v -> ParseTree scheme r
  -- | Abstracts '(<|>)'
  SumNode :: ParseTree scheme r -> ParseTree scheme r -> ParseTree scheme r
  -- | Abstracts 'many' (@MaybeNode False@) and 'some' (@MaybeNode True@)
  ManyNode :: Bool -> ParseTree scheme r -> ParseTree scheme [r]

instance Functor p => Functor (ParseTree p) where
  fmap _ EmptyNode          = EmptyNode
  fmap f (ValueNode value)  = ValueNode $ f value
  fmap f (ParseNode parser) = ParseNode $ fmap f parser
  fmap f (ProdNode g l r)   = ProdNode (\u v -> f $ g u v) l r
  fmap f (SumNode l r)      = SumNode (fmap f l) (fmap f r)
  fmap f node               = liftA2 ($) (pure f) node
  -- This takes advantage of the fact that f <$> x = pure f <*> x.

instance Functor p => Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Functor p => Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode False
  some = ManyNode True

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError EmptyError
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = sumResults (resolve l) (resolve r)
  resolve (ManyNode False _) = pure []
  resolve (ManyNode True  p) = pure <$> resolve p
  -- NOTE: If a ManyNode contains a resolvable node, one might expect
  -- the result to be an infinite list (e.g. `resolve $ many
  -- (ValueNode 1)` to give `Right [1,1,1,1,..]`) or for the
  -- computation to diverge (as is the case for `many (Just 1)`).
  -- However, by only attempting at most resolutions of the subtree,
  -- we will get either zero or one results. For example, `resolve $
  -- many (ValueNode 1)` will give `Right []`.
  --
  -- Whether this is the best possible way to handle the situation is
  -- unclear. This avoids infinite loops, but might not be the
  -- expected behavior in some unforseen use-case.
