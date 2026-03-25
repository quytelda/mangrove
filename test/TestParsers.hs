{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsers where

import           Control.Applicative
import           Data.Text            (Text)

import           Mangrove.Scheme.Unix
import           Mangrove.TextParser
import           Mangrove.Unix

opt_example_unit :: UnixParser ()
opt_example_unit = option [LongFlag "example"] "" $ pure ()

opt_e_unit :: UnixParser ()
opt_e_unit = option [ShortFlag 'e'] "" $ pure ()

opt_e_param :: UnixParser Text
opt_e_param = option [ShortFlag 'e'] "" $ subparameter defaultParser

opt_f_unit :: UnixParser ()
opt_f_unit = option [ShortFlag 'f'] "" $ pure ()

opt_example_param :: UnixParser Text
opt_example_param = option [LongFlag "example"] "" $ subparameter defaultParser

opt_example_switch :: UnixParser Bool
opt_example_switch = switch [LongFlag "example"] ""

opt_example_param_optional :: UnixParser Text
opt_example_param_optional =
  option [LongFlag "example"] ""
  $ subparameter defaultParser <|> pure "asdf"

param_text :: UnixParser Text
param_text = parameter defaultParser

option_asdf :: UnixParser Text
option_asdf = option ["--asdf", "-a"] "" $ pure "qwer"

command_asdf :: UnixParser Text
command_asdf = command ["asdf"] "" $ pure "qwer"

cmd_example_tree :: UnixParser Text
cmd_example_tree = command ["example"] "" $ command ["asdf"] "" $ pure "qwer"

opt_example_pair :: UnixParser (Int, Int)
opt_example_pair = option ["--example"] "" $ (,)
  <$> subparameter defaultParser
  <*> subparameter defaultParser

opt_example_subopt :: UnixParser Text
opt_example_subopt =
  option ["--example"] "" $ suboption "value" defaultParser

opt_home_create :: UnixParser (Text, Bool)
opt_home_create =
  option ["--home"] "Specify home directory and whether to create it" $ (,)
  <$> subparameter defaultParser
  <*> (suboption "create" defaultParser <|> pure False)

withHelp :: UnixParser r -> UnixParser r
withHelp = addHelpOptions ["--help"]
           "Display help and usage information"
