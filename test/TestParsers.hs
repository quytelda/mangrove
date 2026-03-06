{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsers where

import           Control.Applicative
import           Data.Text           (Text)

import           Test.Hspec

import           ParseTree
import           Result
import           Scheme.Unix
import           Unix

opt_example_unit :: CliParser ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_e_unit :: CliParser ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_e_param :: CliParser Text
opt_e_param = option [ShortFlag 'e'] "" defaultParameter

opt_f_unit :: CliParser ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: CliParser Text
opt_example_param = option [LongFlag "example"] "" defaultParameter

opt_example_switch :: CliParser Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: CliParser Text
opt_example_param_optional =
  option [LongFlag "example"] "" (defaultParameter <|> pure "asdf")

param_text :: CliParser Text
param_text = defaultParameter

option_asdf :: CliParser Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: CliParser Text
command_asdf = command ["asdf"] "" $ pure "qwer"

cmd_example_tree :: CliParser Text
cmd_example_tree = command ["example"] "" $ command ["asdf"] "" $ pure "qwer"

testParser :: (Eq r, Show r) => CliParser r -> [Text] -> r -> Expectation
testParser tree args expect = parseArguments tree args `shouldBe` Success (expect, [])
