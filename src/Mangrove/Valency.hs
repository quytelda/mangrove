module Mangrove.Valency
  ( Valency(..)
  , multary
  ) where

-- | Valency represents the maximum number of arguments a parsing
-- structure can consume.
--
-- If the valency of a parser is 'Just n', then it might consume up to
-- 'n' arguments. If the valency is 'Nothing', it can consume an
-- arbitrary number of arguments.
class Valency s where
  valency :: s r -> Maybe Int

-- | Multary parsers can consume more than one argument.
multary :: Valency s => s r -> Bool
multary s =
  case valency s of
    Just n  -> n > 1
    Nothing -> True
