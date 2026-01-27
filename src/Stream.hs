{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolymorphicComponents     #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Stream
Copyright   : (c) Quytelda Kahja, 2025
License     : BSD-3-Clause

Provides a basic stream-parsing monad for parsing a token sequences
with error handling and context management.
-}
module Stream
  ( -- * Types
    StreamParser(..)
  , StreamHandler(..)
  , requestHelp

    -- * Context
  , withContext

    -- * Stream
  , popMaybe
  , peekMaybe
  , pop
  , peek
  , push
  , pop_
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)

data StreamHandler tok a r = StreamHandler
  { onSuccess     :: a -> [tok] -> r -- ^ Success Continuation
  , onEmpty       :: [tok] -> r -- ^ Empty continuation
  , onFailure     :: Builder -> r -- ^ Failure Continuation
  , onHelpRequest :: [Text] -> r -- ^ Help Continuation
  } deriving (Functor)

-- | The amazing stream parsing monad! This monad is comparable to a
-- combination of StateT, ExceptT, and MaybeT. It tracks the stream
-- state and handles pure exceptions.
newtype StreamParser tok a = StreamParser
  { runStreamParser
    :: forall r. StreamHandler tok a r
    -> [tok] -- stream content
    -> r
  }

instance Functor (StreamParser tok) where
  fmap f parser = StreamParser $ \handler ->
    runStreamParser parser handler { onSuccess = onSuccess handler . f }

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \handler -> onSuccess handler a
  mf <*> ma = StreamParser $ \handler ->
    runStreamParser mf
    handler { onSuccess = \f -> runStreamParser ma handler { onSuccess = onSuccess handler . f } }

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \handler -> onEmpty handler
  l <|> r = StreamParser $ \handler ->
    runStreamParser l handler { onEmpty = runStreamParser r handler }

instance Monad (StreamParser tok) where
  return = pure
  ma >>= f = StreamParser $ \handler ->
    runStreamParser ma handler { onSuccess = \a -> runStreamParser (f a) handler }

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \handler _ -> onFailure handler err
  catchError ma recover = StreamParser $ \handler ts ->
    runStreamParser ma handler { onFailure = \err -> runStreamParser (recover err) handler ts } ts

requestHelp :: StreamParser tok a
requestHelp = StreamParser $ \handler _ -> onHelpRequest handler []

-- | Push the given context onto the stack, perform a computation,
-- then pop it off. This assumes the computation doesn't modify the
-- stack.
withContext :: Builder -> StreamParser tok a -> StreamParser tok a
withContext context action = StreamParser $ \handler ->
  runStreamParser action handler { onFailure = onFailure handler . prepend context }
  where
    prepend s1 s2 = s1 <> ": " <> s2

--------------------------------------------------------------------------------

-- | Remove and return the first token in the stream.
popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \handler ts ->
  case ts of
    (t:ts') -> onSuccess handler (Just t) ts'
    _       -> onSuccess handler Nothing ts

-- | View the first token in the stream without consuming it.
peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \handler ts ->
  case ts of
    (t:_) -> onSuccess handler (Just t) ts
    _     -> onSuccess handler Nothing ts

-- | Remove and return the first token in the stream. Evaluates to
-- 'empty' if there are no tokens in the stream.
pop :: StreamParser tok tok
pop = StreamParser $ \handler ts ->
  case ts of
    (t:ts') -> onSuccess handler t ts'
    _       -> onEmpty handler ts

-- | View the first token in the stream without consuming it.
-- Evaluates to 'empty' if there are no tokens in the stream.
peek :: StreamParser tok tok
peek = StreamParser $ \handler ts ->
  case ts of
    (t:_) -> onSuccess handler t ts
    _     -> onEmpty handler ts

-- | Prepend a token to the front of the stream.
push :: tok -> StreamParser tok ()
push t = StreamParser $ \handler -> onSuccess handler () . (t:)

-- | Discard the first token in the stream. Nothing happens if there
-- are no tokens in the stream.
pop_ :: StreamParser tok ()
pop_ = StreamParser $ \handler -> onSuccess handler () . drop 1
