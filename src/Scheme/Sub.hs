{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Sub where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           Resolve
import           Scheme
import           Stream
import           Text
import           TextParser

-- | Parsers for subarguments of an option (e.g. @--option key=value@).
data SubScheme r
  = SubParameter (TextParser r) -- ^ Parses freeform arguments
  | SubOption Text (TextParser r) -- ^ Suboptions have the form "KEY=VALUE"
  deriving (Functor)

instance Resolve SubScheme where
  resolve (SubParameter (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText hint]
  resolve (SubOption key (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText key <> "=" <> TLB.fromText hint]
