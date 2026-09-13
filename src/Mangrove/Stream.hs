{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Mangrove.Stream
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing argument sequences
with error handling and context management.
-}

module Mangrove.Stream
  ( -- * Stream Parser
    StreamParser(..)
  , StreamHandler(..)
  , StreamState(..)
  , failure
  , request

    -- ** Escaping
  , setEscaped
  , getEscaped

    -- ** Context
  , getContext
  , setContext
  , withContext
  , formatError

    -- ** Streaming
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_
  , getContent
) where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.List              as List
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           GHC.Generics

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
  { streamContent :: ![Text] -- ^ A sequence of 'Text' values
  , streamContext :: ![tok] -- ^ A stack representing current parsing context
  , streamEscaped :: !Bool -- ^ Escaped mode
  } deriving (Eq, Generic, Show)

-- | A collection of continuations to be called for each situation a
-- stream parser might encounter.
data StreamHandler req tok a r = StreamHandler
  { onSuccess :: StreamState tok -> a -> r -- ^ Success Continuation
  , onEmpty   :: StreamState tok -> r -- ^ Empty continuation
  , onFailure :: StreamState tok -> Builder -> r -- ^ Failure Continuation
  , onRequest :: StreamState tok -> req -> r -- ^ Request Continuation
  } deriving (Functor)

-- | The amazing stream parsing monad! This monad tracks the stream
-- state and context. It short-circuits when exceptions or requests
-- are raised.
newtype StreamParser req tok a = StreamParser
  { runStreamParser
    :: forall r. StreamHandler req tok a r
    -> StreamState tok
    -> r
  }

instance Functor (StreamParser req tok) where
  fmap f parser = StreamParser $ \handler ->
    runStreamParser parser handler { onSuccess = \s -> onSuccess handler s . f }

instance Applicative (StreamParser req tok) where
  pure a = StreamParser $ \handler state -> onSuccess handler state a
  mf <*> ma = StreamParser $ \handler ->
    runStreamParser mf
    handler { onSuccess = \s f -> runStreamParser ma handler { onSuccess = \s' -> onSuccess handler s' . f } s }

instance Alternative (StreamParser req tok) where
  empty = StreamParser $ \handler -> onEmpty handler
  l <|> r = StreamParser $ \handler ->
    runStreamParser l handler { onEmpty = runStreamParser r handler }

instance Monad (StreamParser req tok) where
  return = pure
  ma >>= f = StreamParser $ \handler ->
    runStreamParser ma handler { onSuccess = \s a -> runStreamParser (f a) handler s }

-- | Exit parsing with an error message because something has gone
-- wrong.
failure :: Builder -> StreamParser req tok a
failure err = StreamParser $ \handler state ->
  onFailure handler state err

instance MonadError Builder (StreamParser req tok) where
  throwError = failure
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
setEscaped :: Bool -> StreamParser req tok ()
setEscaped b = StreamParser $ \handler state ->
  onSuccess handler state { streamEscaped = b } ()

-- | Check whether escaped parsing is enabled.
getEscaped :: StreamParser req tok Bool
getEscaped = StreamParser $ \handler state ->
  onSuccess handler state (streamEscaped state)

-- | Signal that information is requested. Short-circuits any further
-- operations.
request :: req -> StreamParser req tok a
request requestType = StreamParser $ \handler state ->
  onRequest handler state requestType

-- | Get a list representing the current context stack.
getContext :: StreamParser req tok [tok]
getContext = StreamParser $ \handler state ->
  onSuccess handler state (streamContext state)

-- | Replace the context stack.
setContext :: [tok] -> StreamParser req tok ()
setContext contexts = StreamParser $ \handler state ->
  onSuccess handler state { streamContext = contexts } ()

-- | Push the provided token onto the context stack, then perform some
-- computation. Afterwards, the stack is restored to its prior state.
withContext :: tok -> StreamParser req tok a -> StreamParser req tok a
withContext context action = do
  oldContext <- getContext
  setContext $ context : oldContext
  action <* setContext oldContext

-- | Format an error message with context information.
formatError :: Render tok => [tok] -> Builder -> Text
formatError contexts err =
  TL.toStrict
  $ TLB.toLazyText
  $ mconcat
  $ List.intersperse ": "
  $ reverse
  $ err : map render contexts

--------------------------------------------------------------------------------

-- | Retrieve the full list of unconsumed input. This doesn't consume
-- anything or alter the state.
getContent :: StreamParser req tok [Text]
getContent = StreamParser $ \handler state ->
  onSuccess handler state $ streamContent state

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser req tok (Maybe Text)
popMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } (Just t)
    _       -> onSuccess handler state Nothing

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser req tok (Maybe Text)
peekMaybe = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state (Just t)
    _     -> onSuccess handler state Nothing

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser req tok Text
pop = StreamParser $ \handler state ->
  case streamContent state of
    (t:ts') -> onSuccess handler state { streamContent = ts' } t
    _       -> onEmpty handler state

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser req tok Text
peek = StreamParser $ \handler state ->
  case streamContent state of
    (t:_) -> onSuccess handler state t
    _     -> onEmpty handler state

-- | Prepend a token to the front of the stream.
push :: Text -> StreamParser req tok ()
push t = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = t : streamContent state }
  ()

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser req tok ()
pop_ = StreamParser $ \handler state ->
  onSuccess handler
  state { streamContent = drop 1 $ streamContent state }
  ()
