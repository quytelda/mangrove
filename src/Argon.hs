{-# LANGUAGE OverloadedStrings #-}

module Argon
  ( CliParser
  , withCliParser
  , handleArguments
  , parameter
  , defaultParameter
  , option
  , optionPure
  , switch
  , command
  , subparameter
  , suboption
  ) where

import           HelpInfo
import           ParseTree
import           Scheme
import           Scheme.Cli
import           Scheme.Sub
import           Text
import           TextParser

import           Control.Applicative
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TLIO
import           System.Environment
import           System.IO

type CliParser = ParseTree CliScheme

withCliParser
  :: CliParser r
  -> (r -> IO a) -- ^ Success handler
  -> (TL.Text -> IO a) -- ^ Error handler
  -> ([Text] -> IO a) -- ^ Help request handler
  -> IO a
withCliParser tree onSuccess onError onHelpRequest = do
  args <- fmap T.pack <$> getArgs
  parseTree tree
    (\result leftover ->
       case leftover of
         []        -> onSuccess result
         (token:_) -> onError $ "unexpected " <> renderLazyText token
    )
    (onError . TLB.toLazyText)
    onHelpRequest
    (parseTokens args)

handleArguments
  :: CliParser r
  -> Text -- ^ Program name
  -> Text -- ^ Program description
  -> (r -> IO ())
  -> IO ()
handleArguments tree programName programDesc run = do
  withCliParser tree
    run
    (TLIO.hPutStrLn stderr)
    (\_ -> TLIO.putStrLn
           $ TLB.toLazyText
           $ renderHelpInfo programName programDesc tree
    )

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
