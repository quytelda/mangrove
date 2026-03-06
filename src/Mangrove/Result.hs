{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mangrove.Result
  ( Result(..)
  ) where

import           Control.Monad.Except

data Result e r
  = Success r
  | Failure e
  | HelpRequest
  deriving (Eq, Show, Functor)

instance Applicative (Result e) where
  pure = Success
  Success f <*> Success a = Success $ f a
  HelpRequest <*> _       = HelpRequest
  _ <*> HelpRequest       = HelpRequest
  Failure e <*> _         = Failure e
  _ <*> Failure e         = Failure e

instance Monad (Result e) where
  return = pure
  Success a >>= f   = f a
  HelpRequest >>= _ = HelpRequest
  Failure e >>= _   = Failure e

instance MonadError e (Result e) where
  throwError = Failure
  catchError (Success a) _         = Success a
  catchError (Failure err) handler = handler err
  catchError HelpRequest _         = HelpRequest
