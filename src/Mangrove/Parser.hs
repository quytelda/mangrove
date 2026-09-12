{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Mangrove.Parser
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

This module contains types and functions necessary for running
argument parsers. These functions are generic across different parsing
schemes.

Most clients won't import this module directly, since its contents are
re-exported by the "Mangrove" module alongside other helpful symbols.
-}
module Mangrove.Parser
  ( -- * Standard Interface
    parseArguments

    -- * Pure Interface
  , Result(..)
  , runArgumentParser
  , runArgumentParser'

    -- * Feeding Parser Trees
  , satiate
  ) where

import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Environment
import           System.Exit
import           System.IO

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Scheme
import           Mangrove.Stream
import           Mangrove.Text
import           Mangrove.Token

--------------------------------------------------------------------------------
-- Feeding ParseTrees

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. When a subtree successfully consumes input, it is
-- replaced with an updated subtree and the traversal ceases.
feed :: Scheme s => ParseTree s r -> StreamParser (Request s) (Token s) (ParseTree s r)
feed EmptyNode = empty
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
satiate :: Scheme s => ParseTree s r -> StreamParser (Request s) (Token s) (ParseTree s r)
satiate tree = do
  parseSpecials
  result <- optional $ feed tree
  case result of
    Just tree' -> satiate tree'
    Nothing    -> pure tree

--------------------------------------------------------------------------------
-- Running Parsers

-- | The result of an argument parsing operation.
data Result req a
  = Success ![Text] !a
  | Failure !Text
  | Request !req
  deriving (Eq, Functor, Show)

-- | Create a default initial t'StreamState' from a list of arguments.
argsToState :: [Text] -> StreamState s
argsToState args = StreamState args [] False

-- | A more general form of 'runArgumentParser' that accepts a custom
-- stream starting state.
runArgumentParser'
  :: Scheme s =>
  ParseTree s r
  -> StreamState (Token s)
  -> Result (Request s) r
runArgumentParser' tree state =
  runStreamParser (satiate tree) handler state
  where
    _onFailure state' = Failure . formatError (streamContext state')
    _onSuccess state' tree' =
      case (streamContent state', resolve tree') of
        (leftovers, Value result) -> Success leftovers result
        ([], EmptyError)          -> _onFailure state' "empty"
        ([], ExpectedError es)    -> _onFailure state' $ renderExpectedError es
        (token:_, _)              -> _onFailure state' $ "unexpected " <> render token
    _onRequest _ req = Request req
    handler = StreamHandler
      { onSuccess = _onSuccess
      , onFailure = _onFailure
      , onEmpty = flip _onFailure "empty"
      , onRequest = _onRequest
      }

-- | Satiate a 'ParseTree' with all the input it can consume, then
-- attempt to evaluate it. Empty results are treated as failures.
runArgumentParser
  :: Scheme s
  => ParseTree s r
  -> [Text]
  -> Result (Request s) r
runArgumentParser tree =
  runArgumentParser' tree . argsToState

-- | Parse the command line arguments passed to the program, then
-- invoke the program's entrypoint with the results of the parsing. If
-- parsing fails, we instead display an error to stderr and exit.
-- Alternatively, if information was requested, we abandon parsing and
-- print the relevant response to stdout, then exit without indicating
-- an error.
parseArguments
  :: Scheme s
  => ProgramInfo s -- ^ Program metadata
  -> ParseTree s r -- ^ Argument parser
  -> (r -> IO a) -- ^ Program Entrypoint
  -> IO a
parseArguments info tree action = do
  args <- map T.pack <$> getArgs
  case runArgumentParser tree args of
    Success [] result -> action result
    Success (token:_) _ -> do
      hPutBuilder stderr $ "unexpected " <> render token <> "\n"
      exitFailure
    Failure err -> do
      TIO.hPutStrLn stderr err
      exitFailure
    Request req -> do
      TIO.putStr $ respond req tree info
      exitSuccess
