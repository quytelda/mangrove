{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mangrove.Resolve
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Resolvable parsers represent expressions that can be evaluated to a
value once they have received the appropriate input.
-}
module Mangrove.Resolve
  ( -- * Resolution
    ResolveError(..)
  , sumResults
  , Resolve(..)
  , resolveLifted
  ) where

import           Control.Monad.Except
import           Data.Bifunctor
import qualified Data.List            as List
import           Mangrove.Text

-- | Resolving an object might fail if not enough input has been
-- provided or the result depends on an unresolvable value (e.g.
-- empty).
data ResolveError
  = EmptyError
  | ExpectedError [Builder]
  deriving (Eq, Show)

instance Semigroup ResolveError where
  ExpectedError ls <> ExpectedError rs = ExpectedError $ ls <> rs
  l <> EmptyError                      = l
  EmptyError <> r                      = r

instance Render ResolveError where
  render EmptyError = "empty"
  render (ExpectedError ts) = "expected: " <> mconcat (List.intersperse " or " ts)

-- | Combine resolution results, preserving information about failures
-- when necessary.
sumResults :: Either ResolveError r -> Either ResolveError r -> Either ResolveError r
sumResults (Left e1) (Left e2) = Left $ e1 <> e2
sumResults l r                 = l <> r

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: f r -> Either ResolveError r

-- | Lift 'resolve' into 'MonadError Builder'.
resolveLifted :: (Resolve f, MonadError Builder m) => f r -> m r
resolveLifted = liftEither . first render . resolve
