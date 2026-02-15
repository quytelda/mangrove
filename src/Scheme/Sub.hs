{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Scheme.Sub
  ( SubScheme(..)
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           Resolve
import           Scheme
import           Stream
import           Text
import           TextParser

-- | Parsers for subarguments of an option, i.e. '--option key=value'.
data SubScheme r
  = SubParameter (TextParser r)
  | SubOption Text (TextParser r)
  deriving (Functor)

instance HasValency SubScheme where
  valency _ = Just 1

instance Resolve SubScheme where
  resolve (SubParameter (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText hint]
  resolve (SubOption key (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText key <> "=" <> TLB.fromText hint]

instance Render (SubScheme r) where
  render = renderParser

instance Scheme SubScheme where
  data Token SubScheme
    = SubAssoc Text Text -- ^ A key=value argument
    | SubArgument Text -- ^ A standard argument
    deriving (Eq, Show)

  renderToken (SubAssoc k v)  = render k <> "=" <> render v
  renderToken (SubArgument s) = render s

  sepProd _ = ","
  sepSum _ = " | "

  renderParser (SubParameter tp)  = render $ parserHint tp
  renderParser (SubOption key tp) = render key <> "=" <> render (parserHint tp)

  activate parser = do
    next <- peek
    escaped <- isEscaped

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
