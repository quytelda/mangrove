module Argon
  ( parameter
  , defaultParameter
  , option
  , optionPure
  , switch
  , command
  , subparameter
  , suboption
  ) where

import           Scheme.Cli
import           Scheme.Sub
import           Scheme.Internal
import           ParseTree

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Text           (Text)

class HasParameter p where
  parameter :: TextParser a -> ParseTree p a

instance HasParameter SubScheme where
  parameter = subparameter

instance HasParameter CliScheme where
  parameter = cliparameter

defaultParameter :: (DefaultParser r, HasParameter p) => ParseTree p r
defaultParameter = parameter defaultParser

-- | Define a command line parameter (i.e. a non-option).
cliparameter :: TextParser a -> ParseTree CliScheme a
cliparameter = ParseNode . CliParameter

-- | Define a standard CLI option triggered by one or more flags.
option
  :: NonEmpty Flag
  -> Text
  -> ParseTree SubScheme a
  -> ParseTree CliScheme a
option flags help = ParseNode . CliOption (OptionInfo flags help)

-- | Define a CLI option which takes no parameter and produces a pure value.
optionPure
  :: NonEmpty Flag
  -> Text
  -> a
  -> ParseTree CliScheme a
optionPure flags help = ParseNode . CliOption (OptionInfo flags help) . pure

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Flag -> Text -> ParseTree CliScheme Bool
switch flags help = optionPure flags help True <|> pure False

-- | Define a CLI subcommand with it's own parsing subtree.
command
  :: NonEmpty Text
  -> Text
  -> ParseTree CliScheme r
  -> ParseTree CliScheme r
command cmds help = ParseNode . CliCommand (CommandInfo cmds help)

-- | Define a subparameter to a CLI option.
subparameter :: TextParser a -> ParseTree SubScheme a
subparameter = ParseNode . SubParameter

-- | Define a suboption to a CLI option.
suboption :: Text -> TextParser a -> ParseTree SubScheme a
suboption key = ParseNode . SubOption key
