{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Mangrove.Scheme.Unix
Copyright   : (c) Quytelda Kahja, 2026
License     : BSD-3-Clause

A parsing scheme for Unix-style command line arguments.
-}
module Mangrove.Scheme.Unix
  ( -- * Describing Commands & Options
    Flag(..)
  , OptionInfo(..)
  , CommandInfo(..)

    -- * Unix Scheme
  , UnixScheme(..)
  , Token(..)
  , UnixParser

    -- * Help
  , addHelpOptions
  , renderHelp
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import qualified Data.List               as List
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Builder  as TLB

import           Mangrove.ArgumentParser
import           Mangrove.ParseTree
import           Mangrove.Resolve
import           Mangrove.Scheme
import           Mangrove.Scheme.Sub     (SubScheme)
import qualified Mangrove.Scheme.Sub     as Sub
import           Mangrove.Stream
import           Mangrove.Text
import           Mangrove.TextParser
import           Mangrove.Valency

--------------------------------------------------------------------------------
-- User Interface Descriptions

-- | A flag is a special argument that identifies a named option to
-- the parser. Flags can have two forms: long flags start with a
-- double dash (e.g. "--example") followed by a string while short
-- flags start with only a single dash (e.g. "-e") and are identified
-- by a single character.
data Flag
  = LongFlag Text
  | ShortFlag Char
  deriving (Eq, Ord, Show)

instance IsString Flag where
  fromString ('-':'-':name)
    | not (null name) && all isAlphaNum name = LongFlag $ T.pack name
  fromString ['-', c]
    | isAlphaNum c = ShortFlag c
  fromString s = error $ "not a valid flag: " <> s

instance Render Flag where
  render (LongFlag s)  = "--" <> render s
  render (ShortFlag c) = "-" <> render c

-- | A description of a CLI option.
data OptionInfo = OptionInfo
  { optFlags :: NonEmpty Flag -- ^ A list of flags that trigger this option.
  , optHelp  :: Text -- ^ A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

-- | A description of a CLI command.
data CommandInfo = CommandInfo
  { cmdNames :: NonEmpty Text -- ^ Command Names
  , cmdHelp  :: Text -- ^ A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

-- | A parser scheme for Unix-style CLI arguments.
data UnixScheme r
  = Parameter (TextParser r) -- ^ A standard freeform parameter
  | Command CommandInfo (ParseTree UnixScheme r) -- ^ A subcommand with its own parse tree
  | Option OptionInfo (ParseTree SubScheme r) -- ^ A named option that
                                              -- might support suboptions
  deriving (Functor)

instance Valency UnixScheme where
  valency (Parameter _)       = Just 1
  valency (Command _ subtree) = fmap (+1) (valency subtree)
  valency (Option _ subtree)  = fmap (max 2) (valency subtree)

instance Resolve UnixScheme where
  resolve (Parameter (TextParser hint _)) =
    throwError $ ExpectedError [render hint]
  resolve (Option info _) =
    throwError $ ExpectedError [render (optHead info)]
  resolve (Command info _) =
    throwError $ ExpectedError [render (cmdHead info)]

parseUnixOption :: Alternative f => Text -> f (Flag, Maybe Text)
parseUnixOption (T.stripPrefix "--" -> Just s)
  | not (T.null s) =
    case keyEqualsValue s of
      Just (k, v) -> pure (LongFlag k, Just v)
      Nothing     -> pure (LongFlag s, Nothing)
parseUnixOption (T.stripPrefix "-" >=> T.uncons -> Just (k,v))
  | k /= '-' =
    pure (ShortFlag k, if T.null v then Nothing else Just v)
parseUnixOption _ = empty

isMarked :: Text -> Bool
isMarked "-" = False
isMarked s   = "-" `T.isPrefixOf` s

instance Scheme UnixScheme where
  data Token UnixScheme
    = UnixArgument Text -- ^ A freeform argument that is not an option or command
    | UnixCommand Text -- ^ A subcommand
    | UnixOption Flag (Maybe Text) -- ^ A named option with optional bound argument
    deriving (Eq, Show)

  delimiter _ = ' '

  parseSpecials = do
    peekMaybe >>= \case
      Just "--" -> pop_ *> setEscaped True
      _         -> pure ()

  activate (Parameter tp) = do
    next <- peek

    -- Arguments that begin with a dash aren't considered free
    -- arguments unless escaping is enabled. However, the string "-"
    -- is always accepted since this is commonly used to represent
    -- stdin.
    escaped <- getEscaped
    guard $ escaped || not (isMarked next)

    withContext (UnixArgument next) $
      pop_ *> runTextParser tp next

  activate (Option info subtree) = do
    -- Arguments should never be interpreted as options when escaped.
    getEscaped >>= guard . not

    (flag, mbound) <- peek >>= parseUnixOption
    guard $ flag `elem` optFlags info
    pop_

    let splitArgs s = if multary subtree
                      then T.split (== ',') s
                      else [s]
        initState args = StreamState
          { streamContent = args
          , streamContext = []
          , streamEscaped = not $ Sub.hasSubOptions subtree
          }
        parseSubargs args =
          case runArgumentParser' subtree (initState args) of
            Success leftover result -> pure (leftover, result)
            Failure contexts err    -> throwError $ renderError contexts err
            HelpRequest _           -> requestHelp

    withContext (UnixOption flag mbound) $ do
      mnext <- peekMaybe
      case (mbound, mnext) of
        (Just argString, _) -> do
          (leftover, result) <- parseSubargs (splitArgs argString)
          forM_ leftover $ \arg ->
            throwError $ "unrecognized subargument: " <> render arg
          pure result
        (_, Just argString)
          | not (isMarked argString) -> do
              let args = splitArgs argString
              (leftover, result) <- parseSubargs args
              when (length args /= length leftover) $ do
                forM_ leftover $ \arg ->
                  throwError $ "unrecognized subargument: " <> render arg
                pop_
              pure result
        _ -> do
          (_, result) <- parseSubargs []
          pure result

  activate (Command info subtree) = do
    -- Arguments should never be interpreted as commands when escaped.
    getEscaped >>= guard . not

    next <- peek
    guard $ next `elem` cmdNames info
      && not ("-" `T.isPrefixOf` next) -- not sure if this check is necessary?
    pop_

    withContext (UnixCommand next) $ do
      satiate subtree
      >>= resolveLifted

  usageInfo (Parameter tp) = render $ parserHint tp
  usageInfo (Command info subtree) =
    "{" <> render (cmdHead info) <> " " <> render subtree <> "}"
  usageInfo (Option info subtree) =
    render flag
    <> if nullary subtree
       then mempty
       else separator <> render subtree
    where flag = optHead info
          separator = case flag of
                        LongFlag _ -> "="
                        _          -> ""

instance Render (Token UnixScheme) where
  render (UnixArgument s)                      = render s
  render (UnixCommand s)                       = render s
  render (UnixOption f@(LongFlag _) (Just v))  = render f <> "=" <> render v
  render (UnixOption f@(LongFlag _) Nothing)   = render f
  render (UnixOption f@(ShortFlag _) (Just v)) = render f <> render v
  render (UnixOption f@(ShortFlag _) Nothing)  = render f

-- | Convenient type alias for Unix-flavored parse trees.
type UnixParser = ParseTree UnixScheme

--------------------------------------------------------------------------------
-- Help

-- | Automatically insert a help option at the top level of the tree
-- and every subcommand tree.
addHelpOptions
  :: NonEmpty Flag
  -> Text
  -> ParseTree UnixScheme r
  -> ParseTree UnixScheme r
addHelpOptions flags desc tree = ParseNode helpOption <|> go tree
  where
    helpOption :: UnixScheme a
    helpOption =
      Option
      (OptionInfo flags desc)
      HelpNode

    go :: ParseTree UnixScheme a -> ParseTree UnixScheme a
    go (ParseNode (Command info subtree)) =
      ParseNode
      $ Command info
      $ ParseNode helpOption <|> go subtree
    go (ProdNode f l r) = ProdNode f (go l) (go r)
    go (SumNode l r) = SumNode (go l) (go r)
    go (ManyNode require p) = ManyNode require (go p)
    go node = node

data OptionHelp = OptionHelp
  { colShorts :: TL.Text -- Column 1
  , colLongs  :: TL.Text -- Column 2
  , colArg    :: TL.Text -- Column 3
  , colDesc   :: TL.Text -- Column 4
  } deriving (Eq, Ord, Show)

makeOptionHelp :: OptionInfo -> ParseTree SubScheme r -> OptionHelp
makeOptionHelp OptionInfo{..} subtree =
  OptionHelp
  { colLongs  = fmtFlagList longs
  , colShorts = fmtFlagList shorts
  , colArg    = if nullary subtree
                then mempty
                else renderLazyText subtree
  , colDesc   = TL.fromStrict optHelp
  }
  where
    isLongFlag LongFlag{} = True
    isLongFlag _          = False
    (longs, shorts) = NonEmpty.partition isLongFlag optFlags
    fmtFlagList = TL.intercalate ", " . fmap renderLazyText

-- | Enumerate descriptive information for all options available in a
-- parse tree, indexed by the set of commands under which they exist.
collectOptions :: ParseTree UnixScheme r -> Map [CommandInfo] [OptionHelp]
collectOptions tree = go tree mempty
  where
    go :: ParseTree UnixScheme r
       -> Map [CommandInfo] [OptionHelp]
       -> Map [CommandInfo] [OptionHelp]
    go (ParseNode (Option info subtree)) =
      Map.insertWith (<>) [] [makeOptionHelp info subtree]
    go (ParseNode (Command info subtree)) =
      Map.union $ Map.mapKeys (info :) $ collectOptions subtree
    go (ProdNode _ l r) = go r . go l
    go (SumNode l r)    = go r . go l
    go (ManyNode _ p)   = go p
    go _                = id

renderOptionTable :: [OptionHelp] -> Builder
renderOptionTable xs = foldMap formatRow $ List.sort xs
  where
    maxLengthBy f = maximum $ TL.length . f <$> xs
    col1width = maxLengthBy colShorts
    col2width = maxLengthBy colLongs
    col3width = maxLengthBy colArg

    formatRow OptionHelp{..} =
      TLB.fromLazyText $ TL.intercalate "  "
      [ TL.justifyLeft col1width ' ' colShorts
      , TL.justifyLeft col2width ' ' colLongs
      , TL.justifyLeft col3width ' ' colArg
      , colDesc
      , "\n"
      ]

renderHeader :: [CommandInfo] -> Builder
renderHeader [] = mempty
renderHeader cmds@(info : _) =
  fmtCommand cmds
  <> " command"
  <> aliasInfo
  <> ": "
  <> render (cmdHelp info)
  <> "\n"
  where
    quote m = "\"" <> m <> "\""
    fmtCommand = quote . render . T.unwords . fmap cmdHead . reverse
    aliases = NonEmpty.tail $ cmdNames info
    aliasInfo =
      if null aliases
      then mempty
      else " (alt: " <> render (T.intercalate ", " aliases) <> ")"

-- | Format an index of commands and options for help output display.
renderTables :: Map [CommandInfo] [OptionHelp] -> Builder
renderTables =
  Map.foldlWithKey
  (\acc cmds desc ->
      acc
      <> "\n"
      <> renderHeader cmds
      <> renderOptionTable desc
  ) mempty

-- | Select only the options tables which exist under a particular
-- command sequence.
selectSubtable
  :: [Text]
  -> Map [CommandInfo] [OptionHelp]
  -> Map [CommandInfo] [OptionHelp]
selectSubtable cmds table =
  Map.filterWithKey (\infos _ -> isParentCommand cmds infos) table

isParentCommand :: [Text] -> [CommandInfo] -> Bool
isParentCommand cmds =
  and . zipWith (\cmd info -> cmd `elem` cmdNames info) cmds

-- | Render formatted help information for all commands and options
-- that exist underneath the current command context.
renderHelp
  :: ParseTree UnixScheme r
  -> [Token UnixScheme] -- ^ Context Stack
  -> Builder
renderHelp tree contexts =
  renderTables
  $ selectSubtable commandContext
  $ collectOptions tree
  where
    commandContext = reverse [s | UnixCommand s <- contexts]
