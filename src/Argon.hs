module Argon where

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Text           (Text)

import           ParseTree
import           Scheme.Sub
import           Scheme.Unix
import           TextParser

type CliParser = ParseTree UnixScheme

class AcceptsParameters s where
  parameter :: TextParser a -> ParseTree s a

instance AcceptsParameters SubScheme where
  parameter = subparameter

instance AcceptsParameters UnixScheme where
  parameter = unixparameter

-- | Define a command line parameter (i.e. a non-option).
unixparameter :: TextParser a -> ParseTree UnixScheme a
unixparameter = ParseNode . Parameter

defaultParameter :: (DefaultParser r, AcceptsParameters p) => ParseTree p r
defaultParameter = parameter defaultParser

option
  :: NonEmpty Flag
  -> Text
  -> ParseTree SubScheme r
  -> ParseTree UnixScheme r
option flags help = ParseNode . Option (OptionInfo flags help) False

-- | Define a CLI option which takes no parameter and produces a pure value.
optionPure
  :: NonEmpty Flag
  -> Text
  -> a
  -> ParseTree UnixScheme a
optionPure flags help = ParseNode . Option (OptionInfo flags help) False . pure

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Flag -> Text -> ParseTree UnixScheme Bool
switch flags help = optionPure flags help True <|> pure False

-- | Define a CLI subcommand with it's own parsing subtree.
command
  :: NonEmpty Text
  -> Text
  -> ParseTree UnixScheme r
  -> ParseTree UnixScheme r
command cmds help = ParseNode . Command (CommandInfo cmds help)

-- | Define a subparameter to a CLI option.
subparameter :: TextParser a -> ParseTree SubScheme a
subparameter = ParseNode . SubParameter

-- | Define a suboption to a CLI option.
suboption :: Text -> TextParser a -> ParseTree SubScheme a
suboption key = ParseNode . SubOption key
