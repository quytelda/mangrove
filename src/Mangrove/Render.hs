{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Mangrove.Render
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Facilities for textual representation of data structures.
-}
module Mangrove.Render
  ( -- * Text Rendering
    Render(..)
  , renderLazyText
  , renderText
  , putBuilder
  , hPutBuilder

    -- * Helpers & Combinators
  , munless
  , between
  , brackets
  , braces
  , quotes
  , renderDelimitedIf
  , wrapIf
  , bracesIf
  , bracketsIf

    -- * Usage
  , Usage(..)

    -- * Re-exports
  , Builder
  ) where

import           Data.Kind
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TLIO
import           System.IO

-- | A class for things that can be rendered to a text 'Builder'.
class Render a where
  render :: a -> Builder

instance Render Builder where
  render = id

instance Render T.Text where
  render = TLB.fromText

instance Render Char where
  render = TLB.singleton

instance Render String where
  render = TLB.fromString

-- | Convert renderable data directly to lazy 'TL.Text'.
renderLazyText :: Render a => a -> TL.Text
renderLazyText = TLB.toLazyText . render

-- | Convert renderable data directly to strict 'T.Text'.
renderText :: Render a => a -> Text
renderText = TL.toStrict . TLB.toLazyText . render

-- | Write the contents of a 'Builder' to standard output.
putBuilder :: Builder -> IO ()
putBuilder = TLIO.putStr . TLB.toLazyText

-- | Write the contents of a 'Builder' to some IO handle.
hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder handle = TLIO.hPutStr handle . TLB.toLazyText

--------------------------------------------------------------------------------
-- Combinators

-- | Similar to 'Control.Monad.unless', but for monoids.
munless :: Monoid m => Bool -> m -> m
munless condition m = if condition then mempty else m

-- | @between open close s@ surrounds @s@ with @open@ and @close@
-- (i.e. @open <> s <> close@).
between :: Monoid m => m -> m -> m -> m
between open close s = open <> s <> close

-- | Surround a string with square brackets.
brackets :: Builder -> Builder
brackets = between "[" "]"

-- | Surround a string with curly braces.
braces :: Builder -> Builder
braces = between "{" "}"

-- | Surround a string with double quotes.
quotes :: Builder -> Builder
quotes = between "\"" "\""

wrapIf :: (Builder -> Builder) -> Bool -> Builder -> Builder
wrapIf wrap True  = wrap
wrapIf _    False = id

-- | Optionally wrap an expression with braces
bracesIf :: Bool -> Builder -> Builder
bracesIf = wrapIf braces

-- | Optionally wrap an expression with brackets
bracketsIf :: Bool -> Builder -> Builder
bracketsIf = wrapIf brackets

-- | @renderDelimitedIf wrap f x@ will render @x@ as a 'Builder'. If
-- the condition @f x@ is @True@, the result will be modified using
-- the function @wrap@, otherwise the result will be returned
-- unmodified.
renderDelimitedIf :: Render a => (Builder -> Builder) -> (a -> Bool) -> a -> Builder
renderDelimitedIf wrap f x = (if f x then wrap else id) (render x)

--------------------------------------------------------------------------------
-- Usage

-- | A class for parsers that support generation of usage information.
class Usage (p :: Type -> Type) where

  -- | Generate nicely formatted usage information for a parser. This
  -- similar to 'showsPrec'.
  usagePrec :: Int -> p r -> Builder
  usagePrec _ = usage

  -- | Generate nicely formatted usage information for a parser.
  --
  -- Default implementation is equivalent to @usagePrec 0@.
  usage :: p r -> Builder
  usage = usagePrec 0
