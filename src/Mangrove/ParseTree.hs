{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : Mangrove.ParseTree
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A 'ParseTree' is a tree-shaped parser that "filter-feeds" on a stream
of arguments, collecting inputs at the leaves and feeding the results
up the tree for processing. 'ParseTree's are parameterized by the
parser scheme that determines the kind of inputs it accepts.
-}
module Mangrove.ParseTree
  ( -- * Types
    ParseTree(..)
  , nullary

    -- * Feeding Trees
  , satiate

    -- * Generic Parameters
  , AcceptsParameters(..)
  , defaultParameter
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import           Data.Proxy

import           Mangrove.Resolve
import           Mangrove.Scheme
import           Mangrove.Stream
import           Mangrove.Text
import           Mangrove.TextParser
import           Mangrove.Valency

-- | `ParseTree scheme r` is an expression tree composed of parsers
-- from scheme @scheme@ which evaluates to a value of type @r@ when
-- supplied with the proper input.
data ParseTree (scheme :: Type -> Type) (r :: Type) where
  -- | Terminal node with no value (abstracts 'empty')
  EmptyNode :: ParseTree scheme r
  -- | A control node that triggers a help request when reached
  HelpNode :: ParseTree scheme r
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
  fmap _ HelpNode           = HelpNode
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

instance Valency s => Valency (ParseTree s) where
  valency EmptyNode         = Just 0
  valency HelpNode          = Just 0
  valency (ValueNode _)     = Just 0
  valency (ParseNode p)     = valency p
  valency (ProdNode _ l r)  = (+) <$> valency l <*> valency r
  valency (SumNode l r)     = max <$> valency l <*> valency r
  valency (ManyNode _ tree) =
    case valency tree of
      Just n | n <= 0 -> Just 0
      _               -> Nothing -- i.e. infinity
  -- In the above case of 'ManyNode _ p', a ManyNode can accept an
  -- arbitrary number of parameters, so the maximum valency is either
  -- infinite or zero depending on whether the valency of 'p' is zero.

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError EmptyError
  resolve HelpNode           = throwError EmptyError
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

instance Scheme s => Render (ParseTree s r) where
  -- special cases
  render (SumNode p (ValueNode _)) = "[" <> render p <> "]"

  render EmptyNode                 = "EMPTY"
  render HelpNode                  = "HELP"
  render (ValueNode _)             = "VALUE"
  render (ParseNode parser)        = usageInfo parser
  render (ProdNode _ l r)          = render l <> render (delimiter (Proxy @s)) <> render r
  render (SumNode l r)             = render l <> "|" <> render r
  render (ManyNode False p)        = "[" <> render p <> "...]"
  render (ManyNode True p)         = render p <> "..."

-- | Identify trees that do not accept any input.
--
-- A "nullary" tree contains no parsers (i.e. no 'ParseNode'
-- children), and thus cannot accept any input. This does NOT include
-- trees that accept input optionally or trees that only accept
-- impossible input.
nullary :: ParseTree s r -> Bool
nullary EmptyNode         = True
nullary HelpNode          = True
nullary (ValueNode _)     = True
nullary (ParseNode _)     = False
nullary (ProdNode _ l r)  = nullary l && nullary r
nullary (SumNode l r)     = nullary l && nullary r
nullary (ManyNode _ tree) = nullary tree

--------------------------------------------------------------------------------

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. When a subtree successfully consumes input, it is
-- replaced with an updated subtree and the traversal ceases.
feed :: Scheme s => ParseTree s r -> StreamParser (Token s) (ParseTree s r)
feed EmptyNode = empty
feed HelpNode = requestHelp
feed (ValueNode _) = empty
feed (ParseNode parser) = ValueNode <$> activate parser
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) = feed l <|> feed r
feed (ManyNode _ tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode False tree)

-- | Repeatedly traverse the tree, each time activating the first
-- parser that can consume available input, until no more input can be
-- consumed.
satiate :: Scheme s => ParseTree s r -> StreamParser (Token s) (ParseTree s r)
satiate tree = do
  parseSpecials
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

--------------------------------------------------------------------------------
-- Generics

-- | A typeclass for parser schemes that accepts some kind of freeform
-- parameter.
class AcceptsParameters s where
  parameter :: TextParser a -> ParseTree s a

-- | A parameter that uses the 'DefaultParser' implementation to
-- automatically provide a parser for this expected result type.
defaultParameter :: (AcceptsParameters p, DefaultParser r) => ParseTree p r
defaultParameter = parameter defaultParser
