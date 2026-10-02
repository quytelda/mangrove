{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns #-}

module Mangrove.UnixSpec where

import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Version

import           Test.Hspec

import           Mangrove
import           Mangrove.Scheme
import           Mangrove.TextParser
import           Mangrove.Unix

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
      command ["get"]
      "Set a config value"
      parseCfgGetSettings
    cmd_set =
      command ["set"]
      "Set a config value"
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
      command ["delete"]
      "Delete a new user"
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

spec :: Spec
spec = do
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

    context "when global options exist" $ do
      it "adds a global options section" $ do
        helpResponse `shouldSatisfy` T.isInfixOf "GLOBAL OPTIONS"

    context "when no global options exist" $ do
      it "has no global options section" $ do
        makeHelpResponse cmd_create programInfo
          `shouldSatisfy` (not . T.isInfixOf "GLOBAL OPTIONS")

    context "when subcommands exist" $ do
      it "adds a subcommands section" $ do
        helpResponse `shouldSatisfy` T.isInfixOf "SUBCOMMANDS"

    context "when no subcommands exist" $ do
      it "has no subcommands section" $ do
        makeHelpResponse parseCreateSettings programInfo
          `shouldSatisfy` (not . T.isInfixOf "SUBCOMMANDS")

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
