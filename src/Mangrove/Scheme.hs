{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Mangrove.Scheme
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A "scheme" is a set of parsers with an associated token type. The
scheme also defines the way the parser handles requests.
-}

module Mangrove.Scheme
  ( Scheme(..)
  , ProgramInfo(..)
  ) where

import           Data.Kind
import           Data.Text          (Text)
import           Data.Version

import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Stream
import           Mangrove.Token

-- | Program metadata for displaying help output.
data ProgramInfo = ProgramInfo
  { programName    :: !Text -- ^ The program name
  , programDesc    :: !Text -- ^ A description of the program
  , programVersion :: !Version -- ^ The program version
  } deriving (Show)

-- | A scheme is a system of parsers and tokens. It parses a sequence
-- of arguments into tokens and values.
class (Functor s, HasTokens s, Resolve s) => Scheme (s :: Type -> Type) where
  -- | What type of requests does this scheme support? This should be
  -- 'Data.Void.Void' if requests are unsupported.
  type Request s

  -- | Generate a response to a request. If requests are unsupported
  -- for this scheme, the implementation of the function should be
  -- 'Data.Void.absurd'.
  respond :: Request s -> ParseTree s r -> ProgramInfo -> Text

  -- | Parse special control arguments that don't represent tokens in
  -- the scheme, but control aspects of how parsing proceeds (e.g.
  -- escaping).
  parseSpecials :: StreamParser (Request s) (Token s) ()
  parseSpecials = pure ()

  -- | 'activate' tries to run a parser on the current input. If the
  -- parser doesn't apply, it consumes nothing and returns empty. If
  -- it does apply, it consumes the relevant input and returns a
  -- result.
  activate :: s r -> StreamParser (Request s) (Token s) r
