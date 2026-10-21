{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mangrove.UnixSpec (spec) where

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Version
import           Test.Hspec

import           Mangrove
import           Mangrove.Render
import           Mangrove.Scheme
import           Mangrove.TextParser
import           Mangrove.Unix

import           TestParsers

--------------------------------------------------------------------------------
-- Example Interface

prm_username :: UnixParser Text
prm_username = parameter defaultParser { parserHint = "USERNAME" }

data CreateSettings = CreateSettings
  { csUid      :: Maybe Int
  , csUsername :: Text
  } deriving (Eq, Show)

parseCreateSettings :: UnixParser CreateSettings
parseCreateSettings = CreateSettings
  <$> optional opt_uid
  <*> prm_username
  where
    opt_uid =
      option ["--uid", "-u"]
      "Specify a user ID"
      $ subparameter defaultParser

data DeleteSettings = DeleteSettings
  { dsDeleteHome :: Bool
  , dsUsername   :: Text
  } deriving (Eq, Show)

parseDeleteSettings :: UnixParser DeleteSettings
parseDeleteSettings = DeleteSettings
  <$> opt_delHome
  <*> prm_username
  where
    opt_delHome =
      switch ["-d"]
      "Delete the user's home directory"

newtype CfgGetSettings = CfgGetSettings Text
  deriving (Eq, Show)

parseCfgGetSettings :: UnixParser CfgGetSettings
parseCfgGetSettings = CfgGetSettings
  <$> parameter defaultParser

data CfgSetSettings = CfgSetSettings Text Text
  deriving (Eq, Show)

parseCfgSetSettings :: UnixParser CfgSetSettings
parseCfgSetSettings = CfgSetSettings
  <$> parameter defaultParser
  <*> parameter defaultParser

data ConfigMode
  = CfgGet CfgGetSettings
  | CfgSet CfgSetSettings
  deriving (Eq, Show)

parseConfigMode :: UnixParser ConfigMode
parseConfigMode =
  CfgGet <$> cmd_get <|>
  CfgSet <$> cmd_set
  where
    cmd_get =
      command ["get", "read"]
      "Read a config value"
      parseCfgGetSettings
    cmd_set =
      command ["set", "write"]
      "Write a config value"
      parseCfgSetSettings

data Mode
  = Create CreateSettings
  | Delete DeleteSettings
  | Config Text ConfigMode
  deriving (Eq, Show)

cmd_create :: UnixParser Mode
cmd_create =
  command ["create"]
  "Create a new user"
  $ Create <$> parseCreateSettings

parseMode :: UnixParser Mode
parseMode =
  cmd_create <|>
  cmd_delete <|>
  cmd_config
  where
    cmd_delete =
      command ["delete", "rm"]
      "Delete a user"
      $ Delete <$> parseDeleteSettings
    cmd_config =
      command ["config"]
      "Manage configuration settings"
      $ Config <$> prm_username <*> parseConfigMode

data Settings = Settings
  { sVerbose :: Bool -- ^ Show extra output
  , sRoot    :: FilePath -- ^ Filesystem root
  , sMode    :: Mode -- ^ Runtime mode
  } deriving (Eq, Show)

parseSettings :: UnixParser Settings
parseSettings =
  addHelpOptions ["--help", "-h"]
  "Display help and usage information"
  $ Settings <$> opt_verbose
             <*> (opt_root <|> pure "/")
             <*> parseMode
  where
    opt_root =
      option ["--root"]
      "Use an alternative filesystem root"
      $ subparameter parseFilePath
    opt_verbose =
      switch ["-v"]
      "Print debugging output"

programInfo :: ProgramInfo
programInfo = ProgramInfo
  { programName = "manage-users"
  , programDesc = "Manage user accounts"
  , programVersion = makeVersion [1,2,0]
  }

--------------------------------------------------------------------------------
-- Tests

optionSpec :: Spec
optionSpec = do
  it "parses long options" $ do
    runArgumentParser opt_example_unit ["--example"]
      `shouldBe` Success [] ()
  it "parses short options" $ do
    runArgumentParser opt_e_unit ["-e"]
      `shouldBe` Success [] ()

  it "parses options in any order" $ do
    runArgumentParser (opt_e_unit *> opt_f_unit) ["-e", "-f"]
      `shouldBe` Success [] ()
    runArgumentParser (opt_e_unit *> opt_f_unit) ["-f", "-e"]
      `shouldBe` Success [] ()

  describe "switches" $ do
    context "when switch is present" $ do
      it "yields True" $ do
        runArgumentParser opt_example_switch ["--example"]
          `shouldBe` Success [] True
    context "when switch is absent" $ do
      it "yields False" $ do
        runArgumentParser opt_example_switch []
          `shouldBe` Success [] False

  context "when a bound argument is provided" $ do
    context "when an argument is expected" $ do
      it "parses the argument" $ do
        runArgumentParser opt_example_param ["--example=qwer"]
          `shouldBe` Success [] "qwer"
        runArgumentParser opt_e_param ["-eqwer"]
          `shouldBe` Success [] "qwer"
    context "when no argument is expected" $ do
      it "parsing fails" $ do
        runArgumentParser opt_example_unit ["--example=qwer"]
          `shouldBe` Failure "--example=qwer: unrecognized subargument: qwer"
        runArgumentParser opt_e_unit ["-eqwer"]
          `shouldBe` Failure "-eqwer: unrecognized subargument: qwer"

  context "when no argument is expected" $ do
    context "when an argument is available" $ do
      it "doesn't consume the argument" $ do
        runArgumentParser opt_example_unit ["--example", "qwer"]
          `shouldBe` Success ["qwer"] ()

  context "when an argument is required" $ do
    it "renders with parameter hint" $ do
      render opt_example_param `shouldBe` "--example=STRING"
      render opt_e_param `shouldBe` "-eSTRING"

    context "when no argument is provided" $ do
      it "fails to parse" $ do
        runArgumentParser opt_example_param ["--example"]
          `shouldBe` Failure "--example: expected: STRING"
    context "when an argument is provided" $ do
      it "the argument is consumed" $ do
        runArgumentParser opt_example_param ["--example", "qwer"]
          `shouldBe` Success [] "qwer"

  context "when an argument is optional" $ do
    it "renders parameter hint in brackets" $ do
      render opt_example_param_optional `shouldBe` "--example=[STRING]"

    context "when no argument is provided" $ do
      it "yields a default value" $ do
        runArgumentParser opt_example_param_optional ["--example"]
          `shouldBe` Success [] "asdf"
      it "does not consume subsequent options" $ do
        runArgumentParser opt_example_param_optional ["--example", "--option"]
          `shouldBe` Success ["--option"] "asdf"
    context "when an argument is provided" $ do
      it "parses the argument" $ do
        runArgumentParser opt_example_param_optional ["--example", "qwer"]
          `shouldBe` Success [] "qwer"

  describe "compound options" $ do
    context "when the subtree accepts multiple arguments" $ do
      it "splits the input by delimiter" $ do
        runArgumentParser opt_example_pair ["--example", "1,3"]
          `shouldBe` Success [] (1,3)
    context "when the subtree can't accept multiple argument" $ do
      it "doesn't split the input by delimiter" $ do
        runArgumentParser opt_example_param ["--example", "1,3"]
          `shouldBe` Success [] "1,3"
        runArgumentParser opt_example_param_optional ["--example", "1,3"]
          `shouldBe` Success [] "1,3"

    context "when the subtree accepts suboptions" $ do
      it "parses key=value pairs" $ do
        runArgumentParser opt_example_subopt ["--example", "value=asdf"]
          `shouldBe` Success [] "asdf"
        runArgumentParser opt_example_subopt ["--example=value=asdf"]
          `shouldBe` Success [] "asdf"
    context "when the subtree can't accept suboptions" $ do
      it "doesn't parse key=value pairs" $ do
        runArgumentParser opt_example_param ["--example", "value=asdf"]
          `shouldBe` Success [] "value=asdf"
        runArgumentParser opt_example_param ["--example=value=asdf"]
          `shouldBe` Success [] "value=asdf"

  describe "help options" $ do
    let isRequest (Request {}) = True
        isRequest _            = False

    context "when a help option is present" $ do
      it "requests help" $ do
        runArgumentParser (withHelp opt_example_unit) ["--help"]
          `shouldSatisfy` isRequest
      it "works for subcommands" $ do
        runArgumentParser (withHelp cmd_example_tree) ["example", "--help"]
          `shouldSatisfy` isRequest
        runArgumentParser (withHelp cmd_example_tree) ["example", "asdf", "--help"]
          `shouldSatisfy` isRequest

    context "when a help option is absent" $ do
      it "doesn't request help" $ do
        runArgumentParser (withHelp opt_example_unit) ["--example"]
          `shouldBe` Success [] ()
        runArgumentParser (withHelp opt_example_unit) []
          `shouldBe` Failure "expected: --help or --example"
      it "isn't activated by escaped options" $ do
        runArgumentParser (withHelp opt_example_unit) ["--", "--help"]
          `shouldBe` Failure "unexpected --help"

generalSpec :: Spec
generalSpec = do
  context "when \"-\" is given as an argument" $ do
    it "parses the string \"-\"" $ do
      runArgumentParser param_text ["-"]
        `shouldBe` Success [] "-"

  context "when \"--\" is present in the argument list" $ do
    it "treats subsequent arguments as free arguments" $ do
      runArgumentParser param_text ["--", "asdf"]
        `shouldBe` Success [] "asdf"
    it "doesn't treat subsequent arguments as options" $ do
      runArgumentParser (option_asdf <|> param_text) ["--", "--asdf"]
        `shouldBe` Success [] "--asdf"
    it "doesn't treat subsequent arguments as commands" $ do
      runArgumentParser (command_asdf <|> param_text) ["--", "asdf"]
        `shouldBe` Success [] "asdf"

  context "when not enough input is provided" $ do
    it "fails to generate a result" $ do
      runArgumentParser param_text []
        `shouldBe` Failure "expected: STRING"

  context "when not all input can be consumed" $ do
    it "returns unconsumed arguments" $ do
      runArgumentParser param_text ["asdf", "qwer"]
        `shouldBe` Success ["qwer"] "asdf"

spec :: Spec
spec = do
  describe "General" generalSpec
  describe "Options" optionSpec
  describe "Help Output" $ do

    let helpResponse = makeHelpResponse parseSettings programInfo
        usageLines = getUsageLines programInfo helpResponse

    it "displays usage lines for each request option" $ do
      length (filter (T.isSuffixOf "--help") usageLines)
        `shouldBe` 6

    it "displays usage lines for each command" $ do
      length (filter (T.isInfixOf " create ") usageLines)
        `shouldBe` 2
      length (filter (T.isInfixOf " delete ") usageLines)
        `shouldBe` 2
      length (filter (T.isInfixOf " config ") usageLines)
        `shouldBe` 5
      length (filter (T.isInfixOf " get ") usageLines)
        `shouldBe` 2
      length (filter (T.isInfixOf " set ") usageLines)
        `shouldBe` 2

    context "when no global options exist" $ do
      it "has no global options section" $ do
        makeHelpResponse cmd_create programInfo
          `shouldSatisfy` (not . T.isInfixOf "GLOBAL OPTIONS")

    context "when global options exist" $ do
      it "adds a global options section" $ do
        helpResponse `shouldSatisfy` T.isInfixOf "GLOBAL OPTIONS"

    context "when no subcommands exist" $ do
      it "has no subcommands section" $ do
        makeHelpResponse parseCreateSettings programInfo
          `shouldSatisfy` (not . T.isInfixOf "SUBCOMMANDS")

    context "when subcommands exist" $ do
      it "adds a subcommands section" $ do
        helpResponse `shouldSatisfy` T.isInfixOf "SUBCOMMANDS"

      it "adds an option table for each subcommand" $ do
        NonEmpty.head <$> getSubcommandTables helpResponse
          `shouldBe` [ "delete, rm"
                     , "create"
                     , "config set, config write"
                     , "config get, config read"
                     , "config"
                     ]

      it "shows the description for each subcommand" $ do
        helpResponse `shouldSatisfy` T.isInfixOf "Read a config value"
        helpResponse `shouldSatisfy` T.isInfixOf "Write a config value"
        helpResponse `shouldSatisfy` T.isInfixOf "Create a new user"
        helpResponse `shouldSatisfy` T.isInfixOf "Delete a user"
        helpResponse `shouldSatisfy` T.isInfixOf "Manage configuration settings"

--------------------------------------------------------------------------------
-- Helper Functions

makeHelpResponse :: UnixParser a -> ProgramInfo -> Text
makeHelpResponse = respond (HelpRequest [])

getUsageLines :: ProgramInfo -> Text -> [Text]
getUsageLines info =
  fmap (T.drop 2)
  . takeWhile (T.isPrefixOf ("  " <> programName info))
  . drop 1
  . T.lines

getSubcommandTables :: Text -> [NonEmpty Text]
getSubcommandTables help =
  go [] $ drop 2 $ dropWhile (/= "SUBCOMMANDS") $ T.lines help
  where
    isHeaderLine text = not $ T.null text || T.isPrefixOf "  " text

    go :: [NonEmpty Text] -> [Text] -> [NonEmpty Text]
    go results [] = results
    go results (line : _lines) =
      let (body, rest) = break isHeaderLine _lines
      in go ((line :| body) : results) rest
