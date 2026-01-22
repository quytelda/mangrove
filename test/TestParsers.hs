{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsers where

import           Control.Applicative
import           Data.Text           (Text)

import           Test.Hspec

import           Argon
import           Scheme.Cli
import           ParseTree
import           Result

opt_example_unit :: ParseTree CliScheme ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_e_unit :: ParseTree CliScheme ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_e_param :: ParseTree CliScheme Text
opt_e_param = option [ShortFlag 'e'] "" defaultParameter

opt_f_unit :: ParseTree CliScheme ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: ParseTree CliScheme Text
opt_example_param = option [LongFlag "example"] "" defaultParameter

opt_example_switch :: ParseTree CliScheme Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: ParseTree CliScheme Text
opt_example_param_optional =
  option [LongFlag "example"] "" (defaultParameter <|> pure "asdf")

param_text :: ParseTree CliScheme Text
param_text = defaultParameter

option_asdf :: ParseTree CliScheme Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: ParseTree CliScheme Text
command_asdf = command ["asdf"] "" $ pure "qwer"

cmd_example_tree :: ParseTree CliScheme Text
cmd_example_tree = command ["example"] "" $ command ["asdf"] "" $ pure "qwer"

testParser :: (Eq r, Show r) => ParseTree CliScheme r -> [Text] -> r -> Expectation
testParser tree args expect = parseArguments tree args `shouldBe` Success (expect, [])
