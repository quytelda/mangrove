{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.ParseTreeSpec (spec) where

import           Control.Applicative
import           Data.Text           (Text)
import           Test.Hspec

import           Mangrove.Scheme.Unix
import           Mangrove.ParseTree
import           Mangrove.Result

import           TestParsers

spec :: Spec
spec = do
  describe "pure" $ do
    it "resolves to the given value" $ do
      parseArguments (ValueNode 'a' :: ParseTree UnixScheme Char) []
        `shouldBe` Success ('a', [])

  describe "liftA2" $ do
    it "combines two values" $ do
      parseArguments (liftA2 (+) (pure 1) (pure 2) :: ParseTree UnixScheme Int) []
        `shouldBe` Success (3, [])

      -- should be equivalent
      parseArguments ((+) <$> pure 1 <*> pure 2 :: ParseTree UnixScheme Int) []
        `shouldBe` Success (3, [])

  describe "empty" $ do
    it "doesn't resolve to any value" $ do
      parseArguments (empty :: ParseTree UnixScheme Char) []
        `shouldBe` Failure "empty"

  describe "(<|>)" $ do
    context "when the left child is resolvable" $ do
      it "resolves as the left child" $ do
        parseArguments (pure "asdf" <|> opt_e_param) []
          `shouldBe` Success ("asdf", [])

        -- When the right child is also resolvable, it should be
        -- ignored.
        parseArguments (pure "asdf" <|> pure "qwer" :: ParseTree UnixScheme Text) []
          `shouldBe` Success ("asdf", [])

    context "when the left child is unresolvable" $ do
      it "resolves as the right child" $ do
        parseArguments (opt_e_param <|> pure "asdf") []
          `shouldBe` Success ("asdf", [])

    context "when one child is triggered" $ do
      it "prunes the other child" $ do
        parseArguments (opt_e_unit <|> opt_f_unit) ["-e", "-f"]
          `shouldBe` Success ((), ["-f"])
        parseArguments (opt_e_unit <|> opt_f_unit) ["-f", "-e"]
          `shouldBe` Success ((), ["-e"])

  describe "many" $ do
    it "parses multiple instances" $ do
      parseArguments (many opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success (["asdf", "qwer", "zxcv"], [])
    it "parses zero instances" $ do
      parseArguments (many opt_e_param) ["blah"]
        `shouldBe` Success ([], ["blah"])

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      parseArguments (many tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success (["asdf", "qwer"], [])

    it "doesn't swallow arguments" $ do
      parseArguments (many $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
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
      parseArguments (some opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success (["asdf", "qwer", "zxcv"], [])
    it "requires at least one instance" $ do
      parseArguments (some opt_e_param) ["blah"]
        `shouldBe` Failure "unexpected blah"

    it "handles compound trees" $ do
      let tree = (opt_f_unit *> opt_e_param) <|> opt_example_param
      parseArguments (some tree) ["-f", "-e", "asdf", "--example", "qwer"]
        `shouldBe` Success (["asdf", "qwer"], [])

    it "doesn't swallow arguments" $ do
      parseArguments (some $ opt_f_unit *> opt_e_param) ["-f", "-e", "asdf", "-f"]
        `shouldBe` Failure "expected: -e"

  describe "optional" $ do
    it "parses exactly one instance" $ do
      parseArguments (optional opt_e_param) ["-e", "asdf", "-e", "qwer", "-e", "zxcv"]
        `shouldBe` Success ( Just "asdf"
                         , [ "-e"
                           , "qwer"
                           , "-e"
                           , "zxcv"
                           ]
                         )
    it "parses zero instances" $ do
      parseArguments (optional opt_e_param) ["blah"]
        `shouldBe` Success (Nothing, ["blah"])
