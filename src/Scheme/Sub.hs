{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Scheme.Sub where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           ParseTree
import           Resolve
import           Scheme
import           Stream
import           Text
import           TextParser
import           Valency

-- | Parsers for subarguments of an option (e.g. @--option key=value@).
data SubScheme r
  = Parameter (TextParser r) -- ^ Parses freeform arguments
  | Option Text (TextParser r) -- ^ Suboptions have the form "KEY=VALUE"
  deriving (Functor)

instance Valency SubScheme where
  valency _ = Just 1

instance Resolve SubScheme where
  resolve (Parameter (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText hint]
  resolve (Option key (TextParser hint _)) =
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
      (Parameter tp, Nothing) ->
        withContext (SubArgument next) $
        pop_ *> runTextParser tp next
      (Option _ _, Nothing) ->
        empty
      (Option key tp, Just (k,v))
        | not escaped && key == k ->
          withContext (SubAssoc k v) $
          pop_ *> runTextParser tp v
      (Parameter tp, Just _)
        | escaped ->
          withContext (SubArgument next) $
          pop_ *> runTextParser tp next
        | otherwise ->
          empty
      _ -> empty

  usageInfo (Parameter tp)  = render $ parserHint tp
  usageInfo (Option key tp) = render key <> "=" <> render (parserHint tp)

instance Render (Token SubScheme) where
  render (SubAssoc key value) = render key <> "=" <> render value
  render (SubArgument value)  = render value

hasSubOptions :: ParseTree SubScheme r -> Bool
hasSubOptions EmptyNode                    = False
hasSubOptions HelpNode                     = False
hasSubOptions (ValueNode _)                = False
hasSubOptions (ParseNode (Parameter _)) = False
hasSubOptions (ParseNode (Option _ _))  = True
hasSubOptions (ProdNode _ l r)             = hasSubOptions l || hasSubOptions r
hasSubOptions (SumNode l r)                = hasSubOptions l || hasSubOptions r
hasSubOptions (ManyNode _ tree)            = hasSubOptions tree
