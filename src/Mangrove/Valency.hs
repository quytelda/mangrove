{-|
Module      : Mangrove.Valency
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

Typeclass and functions for reasoning about the number of arguments a
parser can consume.
-}

module Mangrove.Valency
  ( Valency(..)
  ) where

-- | Valency represents the maximum number of arguments a parsing
-- structure can consume.
--
-- If the valency of a parser is @Just n@, then it might consume up to
-- @n@ arguments. If the valency is 'Nothing', it can consume an
-- arbitrary number of arguments.
class Valency s where
  -- | Compute the maximum valency of a parser.
  valency :: s r -> Maybe Int

  -- | Test whether a parser has zero inputs.
  --
  -- This does NOT include trees that accept input optionally or trees
  -- that only accept impossible input.
  nullary :: s r -> Bool
  nullary s =
    case valency s of
      Just n  -> n <= 0
      Nothing -> False

  -- | Multary parsers can consume more than one argument.
  --
  -- This does not exclude parsers that could potentially accept zero
  -- or one inputs, as long as the maximum number of inputs is greater
  -- than one.
  multary :: s r -> Bool
  multary s =
    case valency s of
      Just n  -> n > 1
      Nothing -> True
