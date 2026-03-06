{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsers where

import           Control.Applicative
import           Data.Text           (Text)

import           Test.Hspec

import           Mangrove.ParseTree
import           Mangrove.Result
import           Mangrove.Scheme.Unix
import           Mangrove.Unix

opt_example_unit :: UnixParser ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_e_unit :: UnixParser ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_e_param :: UnixParser Text
opt_e_param = option [ShortFlag 'e'] "" defaultParameter

opt_f_unit :: UnixParser ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: UnixParser Text
opt_example_param = option [LongFlag "example"] "" defaultParameter

opt_example_switch :: UnixParser Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: UnixParser Text
opt_example_param_optional =
  option [LongFlag "example"] "" (defaultParameter <|> pure "asdf")

param_text :: UnixParser Text
param_text = defaultParameter

option_asdf :: UnixParser Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: UnixParser Text
command_asdf = command ["asdf"] "" $ pure "qwer"

cmd_example_tree :: UnixParser Text
cmd_example_tree = command ["example"] "" $ command ["asdf"] "" $ pure "qwer"

testParser :: (Eq r, Show r) => UnixParser r -> [Text] -> r -> Expectation
testParser tree args expect = parseArguments tree args `shouldBe` Success (expect, [])
