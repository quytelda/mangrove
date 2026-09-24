{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.ParseTreeSpec (spec) where

import           Control.Applicative
import           Data.Text             (Text)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (Result (..))

import           Mangrove
import           Mangrove.ParseTree
import           Mangrove.Scheme.Unix
import           Mangrove.Valency

import           Arbitrary
import           StructureEq
import           TestParsers

--------------------------------------------------------------------------------
-- Functor Laws

prop_fmapIdLaw :: UnixParser Int -> Bool
prop_fmapIdLaw tree = structEq tree (fmap id tree)

prop_fmapComLaw :: UnixParser Int -> Bool
prop_fmapComLaw tree =
  fmap (inc . dbl) tree
  `structEq`
  (fmap inc . fmap dbl) tree
  where
    inc = (1+)
    dbl = (2*)

--------------------------------------------------------------------------------
-- Applicative Laws

prop_applicativeIdLaw
  :: ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_applicativeIdLaw m (ArgList args) =
  runArgumentParser (pure id <*> m) args
  ==
  runArgumentParser m args

prop_applicativeHomLaw
  :: Fun Int Int
  -> Int
  -> ArgList
  -> Bool
prop_applicativeHomLaw (Fn f) x (ArgList args) =
  runArgumentParser (pure f <*> pure x :: UnixParser Int) args
  ==
  runArgumentParser (pure (f x) :: UnixParser Int) args

prop_applicativeIntLaw
  :: ParseTree UnixScheme (Int -> Int)
  -> Int
  -> ArgList
  -> Bool
prop_applicativeIntLaw u y (ArgList args) =
  runArgumentParser (u <*> pure y) args
  ==
  runArgumentParser (pure ($ y) <*> u) args

prop_applicativeComLaw
  :: ParseTree UnixScheme (Int -> Int)
  -> ParseTree UnixScheme (Int -> Int)
  -> ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_applicativeComLaw u v w (ArgList args) =
  runArgumentParser (pure (.) <*> u <*> v <*> w) args
  ==
  runArgumentParser (u <*> (v <*> w)) args

--------------------------------------------------------------------------------

prop_valencyPositive
  :: UnixParser Int
  -> Bool
prop_valencyPositive p =
  all (>= 0) (valency p)

prop_liftA2AddsValencies
  :: UnixParser Int
  -> UnixParser Int
  -> Bool
prop_liftA2AddsValencies l r =
  valency (liftA2 (+) l r) == liftA2 (+) (valency l) (valency r)

prop_altMaxesValency
  :: UnixParser Int
  -> UnixParser Int
  -> Bool
prop_altMaxesValency l r =
  valency (l <|> r) == liftA2 (max) (valency l) (valency r)

prop_altPicksOne
  :: ParseTree UnixScheme Int
  -> ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_altPicksOne l r (ArgList args) =
  resultSum == resultL || resultSum == resultR
  where
    resultL = runArgumentParser l args
    resultR = runArgumentParser r args
    resultSum = runArgumentParser (l <|> r) args

prop_altEmptyIdentity
  :: (ParseTree UnixScheme Int -> ParseTree UnixScheme Int)
  -> ParseTree UnixScheme Int
  -> ArgList
  -> Bool
prop_altEmptyIdentity append tree (ArgList args) =
  runArgumentParser tree args == runArgumentParser (append tree) args

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Functor Instance" $ do
    prop "satisfies identity law"
      prop_fmapIdLaw
    prop "satisfies composition law"
      prop_fmapComLaw

  describe "Applicative Instance" $ do
    prop "satisfies identity law"
      prop_applicativeIdLaw
    prop "satisfies homomorphism law"
      prop_applicativeHomLaw
    prop "satisfies interchange law"
      prop_applicativeIntLaw
    prop "satisfies composition law"
      prop_applicativeComLaw

  describe "Valency Instance" $ do
    prop "valency is always positive"
      prop_valencyPositive

  describe "pure" $ do
    it "resolves to the given value" $ do
      runArgumentParser (ValueNode 'a' :: ParseTree UnixScheme Char) []
        `shouldBe` Success [] 'a'

  describe "liftA2" $ do
    it "combines two pure values" $ do
      runArgumentParser (liftA2 (+) (pure 1) (pure 2) :: ParseTree UnixScheme Int) []
        `shouldBe` Success [] 3

      -- should be equivalent
      runArgumentParser ((+) <$> pure 1 <*> pure 2 :: ParseTree UnixScheme Int) []
        `shouldBe` Success [] 3

    prop "adds valencies"
      prop_liftA2AddsValencies

  describe "empty" $ do
    it "doesn't resolve to any value" $ do
      runArgumentParser (empty :: ParseTree UnixScheme Char) []
        `shouldBe` Failure "empty"

    it "has valency zero" $ do
      valency (empty :: ParseTree UnixScheme Char)
        `shouldBe` Just 0

  describe "(<|>)" $ do
    prop "valency equals the max valency between its children"
      prop_altMaxesValency
    prop "yields the left or the right result"
      prop_altPicksOne
    prop "empty is left identity" $
      prop_altEmptyIdentity (empty <|>)
    prop "empty is right identity" $
      prop_altEmptyIdentity (<|> empty)

    context "when the left child is resolvable" $ do
      it "resolves as the left child" $ do
        runArgumentParser (pure "asdf" <|> opt_e_param) []
          `shouldBe` Success [] "asdf"

        -- When the right child is also resolvable, it should be
        -- ignored.
        runArgumentParser (pure "asdf" <|> pure "qwer" :: ParseTree UnixScheme Text) []
          `shouldBe` Success [] "asdf"

    context "when the left child is unresolvable" $ do
      it "resolves as the right child" $ do
        runArgumentParser (opt_e_param <|> pure "asdf") []
          `shouldBe` Success [] "asdf"

    context "when one child is triggered" $ do
      it "prunes the other child" $ do
        runArgumentParser (opt_e_unit <|> opt_f_unit) ["-e", "-f"]
          `shouldBe` Success ["-f"] ()
        runArgumentParser (opt_e_unit <|> opt_f_unit) ["-f", "-e"]
          `shouldBe` Success ["-e"] ()

  describe "many" $ do
    it "parses multiple instances" $ do
      runArgumentParser (many opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [] ["asdf", "qwer", "zxcv"]
    it "parses zero instances" $ do
      runArgumentParser (many opt_e_param) ["blah"]
        `shouldBe` Success ["blah"] []

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      runArgumentParser (many tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success [] ["asdf", "qwer"]

    it "doesn't swallow arguments" $ do
      runArgumentParser (many $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
        `shouldBe` Failure "expected: -e"
        -- Some attempts at implementing many/some resulted in
        -- arguments being silently swallowed if they were consumed by
        -- a parser inside a ManyNode which didn't receive enough
        -- input to resolve. In some cases this didn't occur until the
        -- second instance of the subtree was triggered. The expected
        -- behavior in this case is to fail with a message about what
        -- input was missing.

  describe "some" $ do
    it "parses multiple instances" $ do
      runArgumentParser (some opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [] ["asdf", "qwer", "zxcv"]
    it "requires at least one instance" $ do
      runArgumentParser (some opt_e_param) ["blah"]
        `shouldBe` Failure "unexpected blah"

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      runArgumentParser (some tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success [] ["asdf", "qwer"]

    it "doesn't swallow arguments" $ do
      runArgumentParser (some $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
        `shouldBe` Failure "expected: -e"

  describe "optional" $ do
    it "parses exactly one instance" $ do
      runArgumentParser (optional opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success [ "-e", "qwer", "-e", "zxcv"] (Just "asdf")
    it "parses zero instances" $ do
      runArgumentParser (optional opt_e_param) ["blah"]
        `shouldBe` Success ["blah"] Nothing
