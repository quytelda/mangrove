{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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

instance Scheme SubScheme where
  data Token SubScheme
    = SubAssoc Text Text -- ^ A "KEY=VALUE" argument
    | SubArgument Text -- ^ A standard freeform argument
    deriving (Eq, Show)

  delimiter _ = ','

  activate parser = do
    next <- peek
    escaped <- getEscaped
    case (parser, keyEqualsValue next) of
      (SubParameter tp, Nothing) ->
        withContext (SubArgument next) $
        pop_ *> runTextParser tp next
      (SubOption _ _, Nothing) ->
        empty
      (SubOption key tp, Just (k,v))
        | not escaped && key == k ->
          withContext (SubAssoc k v) $
          pop_ *> runTextParser tp v
      (SubParameter tp, Just _)
        | escaped ->
          withContext (SubArgument next) $
          pop_ *> runTextParser tp next
        | otherwise ->
          empty
      _ -> empty

  usageInfo (SubParameter tp)  = render $ parserHint tp
  usageInfo (SubOption key tp) = render key <> "=" <> render (parserHint tp)

instance Render (Token SubScheme) where
  render (SubAssoc key value) = render key <> "=" <> render value
  render (SubArgument value)  = render value
