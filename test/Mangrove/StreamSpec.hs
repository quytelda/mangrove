{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.StreamSpec (spec) where

import           Data.Maybe
import           Data.Text                 (Text)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck           hiding (Result (..))
import           Test.QuickCheck.Instances ()

import           Mangrove
import           Mangrove.Scheme.Unix
import           Mangrove.Stream
import           Mangrove.Test.Stream

--------------------------------------------------------------------------------
-- Functor Laws

prop_functorIdLaw
  :: SP_Unix Int
  -> StreamState UnixScheme
  -> Bool
prop_functorIdLaw (SP_Unix m) state =
  runSPU (fmap id m) state == runSPU m state

prop_functorComLaw
  :: Fun Int Int
  -> Fun Int Int
  -> SP_Unix Int
  -> StreamState UnixScheme
  -> Bool
prop_functorComLaw (Fn f) (Fn g) (SP_Unix m) state =
  runSPU (fmap (f . g) m) state == runSPU ((fmap f . fmap g) m) state

--------------------------------------------------------------------------------
-- Monad Laws

prop_monadLeftId
  :: Int
  -> Fun Int (SP_Unix Int)
  -> StreamState UnixScheme
  -> Bool
prop_monadLeftId a fn state =
  runSPU (f a) state == runSPU (return a >>= f) state
  where
    f = getSPU . applyFun fn

prop_monadRightId
  :: SP_Unix Int
  -> StreamState UnixScheme
  -> Bool
prop_monadRightId (SP_Unix m) state =
  runSPU m state == runSPU (m >>= return) state

prop_monadAssoc
  :: SP_Unix Int
  -> Fun Int (SP_Unix Int)
  -> Fun Int (SP_Unix Int)
  -> StreamState UnixScheme
  -> Bool
prop_monadAssoc (SP_Unix m) fn1 fn2 state =
  runSPU ((m >>= f) >>= g) state == runSPU (m >>= (\x -> f x >>= g)) state
  where
    f = getSPU . applyFun fn1
    g = getSPU . applyFun fn2

--------------------------------------------------------------------------------

prop_peek_preservesState
  :: StreamState UnixScheme
  -> Bool
prop_peek_preservesState state =
  case runSPU peek state of
    (_, state') -> state == state'

prop_pop_preservesContext
  :: StreamState UnixScheme
  -> Bool
prop_pop_preservesContext state =
  case runSPU pop state of
    (_, state') -> streamContext state == streamContext state'

prop_pop_preservesEscaped
  :: StreamState UnixScheme
  -> Bool
prop_pop_preservesEscaped state =
  case runSPU pop state of
    (_, state') -> streamEscaped state == streamEscaped state'

prop_yieldsValueOrEmpty
  :: StreamParser UnixScheme Text
  -> StreamState UnixScheme
  -> Bool
prop_yieldsValueOrEmpty action state =
  case runSPU action state of
    (SPSuccess a, _) -> listToMaybe (streamContent state) == Just a
    (SPEmpty, _)     -> null $ streamContent state
    _                -> False

prop_consumesValue
  :: StreamParser UnixScheme a
  -> StreamState UnixScheme
  -> Bool
prop_consumesValue action state =
  case runSPU action state of
    (_, state') -> streamContent state' == drop 1 (streamContent state)

spec :: Spec
spec = do
  describe "Functor instance" $ do
    prop "satisfies identity law"
      prop_functorIdLaw
    prop "satisfies composition law"
      prop_functorComLaw

  describe "Monad instance" $ do
    prop "satisfies left identity law"
      prop_monadLeftId
    prop "satisfies right identity law"
      prop_monadRightId
    prop "satisfies associativity law"
      prop_monadAssoc

  describe "peek" $ do
    prop "preserves the stream state"
      prop_peek_preservesState
    prop "yields first value or empty" $
      prop_yieldsValueOrEmpty peek

  describe "pop" $ do
    prop "consumes values" $
      prop_consumesValue pop
    prop "yields first value or empty" $
      prop_yieldsValueOrEmpty pop
    prop "preserves escaped setting" $
      prop_pop_preservesEscaped
    prop "preserves the stream state"
      prop_pop_preservesContext
