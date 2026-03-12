module Mangrove.Unix where

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Text           (Text)

import           Mangrove.ParseTree
import qualified Mangrove.Scheme.Sub          as Sub
import           Mangrove.Scheme.Sub          (SubScheme)
import qualified Mangrove.Scheme.Unix         as Unix
import           Mangrove.Scheme.Unix         (UnixScheme)
import           Mangrove.TextParser

type UnixParser = ParseTree UnixScheme

option
  :: NonEmpty Unix.Flag
  -> Text
  -> ParseTree SubScheme r
  -> ParseTree UnixScheme r
option flags help = ParseNode . Unix.Option (Unix.OptionInfo flags help)

-- | Define a CLI option which takes no parameter and produces a pure value.
optionPure
  :: NonEmpty Unix.Flag
  -> Text
  -> a
  -> ParseTree UnixScheme a
optionPure flags help = ParseNode . Unix.Option (Unix.OptionInfo flags help) . pure

-- | Define a CLI option which produces 'True' if present and 'False'
-- otherwise.
switch :: NonEmpty Unix.Flag -> Text -> ParseTree UnixScheme Bool
switch flags help = optionPure flags help True <|> pure False

-- | Define a CLI subcommand with it's own parsing subtree.
command
  :: NonEmpty Text
  -> Text
  -> ParseTree UnixScheme r
  -> ParseTree UnixScheme r
command cmds help = ParseNode . Unix.Command (Unix.CommandInfo cmds help)

-- | Define a subparameter to a CLI option.
subparameter :: TextParser a -> ParseTree SubScheme a
subparameter = ParseNode . Sub.Parameter

-- | Define a suboption to a CLI option.
suboption :: Text -> TextParser a -> ParseTree SubScheme a
suboption key = ParseNode . Sub.Option key
