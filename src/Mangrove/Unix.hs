{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Mangrove.Unix where

import           Control.Applicative
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.Environment
import           System.Exit
import           System.IO

import           Mangrove.ArgumentParser
import           Mangrove.ParseTree
import           Mangrove.Scheme.Sub     (SubScheme)
import qualified Mangrove.Scheme.Sub     as Sub
import           Mangrove.Scheme.Unix    (UnixScheme)
import qualified Mangrove.Scheme.Unix    as Unix
import           Mangrove.Stream
import           Mangrove.Text
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

-- | Parse the command line arguments passed to the program, then
-- invoke the program's entrypoint with the results of the parsing. If
-- parsing fails, we instead display an error to stderr and exit.
-- Alternatively, if help was requested, we abandon parsing and print
-- the relevant help output to stdout, then exit without indicating an
-- error.
parseArguments
  :: UnixParser r
  -> Text -- ^ Program Name
  -> Text -- ^ Program Description
  -> (r -> IO a) -- ^ Program Entrypoint
  -> IO a
parseArguments tree name description action = do
  args <- map T.pack <$> getArgs
  case runArgumentParser tree args of
    Success [] result -> action result
    Success (token:_) _ -> do
      hPutBuilder stderr $ "unexpected " <> render token <> "\n"
      exitFailure
    Failure contexts err -> do
      hPutBuilder stderr $ renderError contexts err <> "\n"
      exitFailure
    HelpRequest contexts -> do
      let cmds = [s | Unix.UnixCommand s <- contexts]
          subIndex = Unix.selectSubtable (reverse cmds) helpIndex
      putBuilder
        $ "Usage: " <> render name <> " " <> render tree <> "\n\n"
        <> render description <> "\n"
        <> Unix.renderTables subIndex
      exitSuccess
  where
    helpIndex = Unix.collectOptions tree
