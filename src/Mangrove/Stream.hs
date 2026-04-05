{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Mangrove.Stream
Copyright   : (c) Quytelda Kahja, 2025
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing argument sequences
with error handling and context management.
-}
module Mangrove.Stream
  ( -- * Types
    StreamParser(..)
  , StreamHandler(..)
  , StreamState(..)

    -- * Monadic Actions
    -- ** Help
  , requestHelp

    -- ** Escaping
  , setEscaped
  , getEscaped

    -- ** Context
  , getContext
  , setContext
  , withContext
  , renderError

    -- ** Streaming
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_
  ) where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.List            as List
import           Data.Text            (Text)

import           Mangrove.Text

-- | The current state of a stream parser.
--
-- The content of a stream is just a list of 'Text' values. The
-- context stack is a list of tokens currently being processed; when a
-- token is recognized, it gets added to front of the list while the
-- token is being parsed into a usable value. When this parsing
-- completes, the token is popped from the front of the list.
--
-- A streams can also enable "escaped" mode by setting 'streamEscaped'
-- to 'True'. What this actually does is parser-dependant, but usually
-- it restricts how subsequent arguments can be interpreted. For
-- example, in the Unix scheme, escaping forces all subsequent
-- arguments to be interpreted as positional arguments, even if they
-- would normally be interpreted as options or commands.
data StreamState tok = StreamState
  { streamContent :: [Text] -- ^ A sequence of 'Text' values
  , streamContext :: [tok]  -- ^ A stack representing current parsing context
  , streamEscaped :: Bool   -- ^ Escaped mode
  } deriving (Eq, Show)

-- | A collection of continuations to be called for each situation a
-- stream parser might encounter.
data StreamHandler tok a r = StreamHandler
  { onSuccess     :: StreamState tok -> a -> r -- ^ Success Continuation
  , onEmpty       :: StreamState tok -> r -- ^ Empty continuation
  , onFailure     :: StreamState tok -> Builder -> r -- ^ Failure Continuation
  , onHelpRequest :: StreamState tok -> r -- ^ Help Continuation
  } deriving (Functor)

-- | The amazing stream parsing monad! This monad tracks the stream
-- state and context. It short-circuits when exceptions or
-- help-requests are raised.
newtype StreamParser tok a = StreamParser
  { runStreamParser
    :: forall r. StreamHandler tok a r
    -> StreamState tok
    -> r
  }

instance Functor (StreamParser tok) where
  fmap f parser = StreamParser $ \handler ->
    runStreamParser parser handler { onSuccess = \s -> onSuccess handler s . f }

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \handler state -> onSuccess handler state a
  mf <*> ma = StreamParser $ \handler ->
    runStreamParser mf
    handler { onSuccess = \s f -> runStreamParser ma handler { onSuccess = \s' -> onSuccess handler s' . f } s }

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \handler -> onEmpty handler
  l <|> r = StreamParser $ \handler ->
    runStreamParser l handler { onEmpty = runStreamParser r handler }

instance Monad (StreamParser tok) where
  return = pure
  ma >>= f = StreamParser $ \handler ->
    runStreamParser ma handler { onSuccess = \s a -> runStreamParser (f a) handler s }

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \handler state -> onFailure handler state err
  catchError ma recover = StreamParser $ \handler state ->
    runStreamParser ma
    handler { onFailure = \_ err -> runStreamParser (recover err) handler state }
    state

-- | Enable or disable escaped parsing. What this actually does is
-- parser-dependant, but usually it restricts how subsequent arguments
-- can be interpreted. For example, in the Unix scheme, escaping
-- forces all subsequent arguments to be interpreted as positional
-- arguments, even if they would normally be interpreted as options or
-- commands.
setEscaped :: Bool -> StreamParser tok ()
setEscaped b = StreamParser $ \handler state ->
  onSuccess handler state { streamEscaped = b } ()

-- | Check whether escaped parsing is enabled.
getEscaped :: StreamParser tok Bool
getEscaped = StreamParser $ \handler state ->
  onSuccess handler state (streamEscaped state)

-- | Signal that help information is requested. Short-circuits any
-- further operations.
requestHelp :: StreamParser tok a
requestHelp = StreamParser onHelpRequest

-- | Get a list representing the current context stack.
getContext :: StreamParser tok [tok]
getContext = StreamParser $ \handler state ->
  onSuccess handler state (streamContext state)

-- | Replace the context stack.
setContext :: [tok] -> StreamParser tok ()
setContext contexts = StreamParser $ \handler state ->
  onSuccess handler state { streamContext = contexts } ()

-- | Push the provided token onto the context stack, then perform some
-- computation. Afterwards, the stack is restored to its prior state.
withContext :: tok -> StreamParser tok a -> StreamParser tok a
withContext context action = do
  oldContext <- getContext
  setContext $ context : oldContext
  action <* setContext oldContext

-- | Format an error message with context information.
renderError :: Render tok => [tok] -> Builder -> Builder
renderError contexts err =
  mconcat
  $ List.intersperse ": "
  $ reverse
  $ err : map render contexts

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe Text)
popMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } (Just t)
    _       -> onSuccess handler state Nothing

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe Text)
peekMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state (Just t)
    _     -> onSuccess handler state Nothing

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok Text
pop = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } t
    _       -> onEmpty handler state

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok Text
peek = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state t
    _     -> onEmpty handler state

-- | Prepend a token to the front of the stream.
push :: Text -> StreamParser tok ()
push t = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = t : streamContent state }
  ()

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = drop 1 $ streamContent state }
  ()
