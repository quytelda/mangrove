{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Scheme.Unix where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T

import           ParseTree
import           Resolve
import           Scheme
import           Scheme.Sub
import           Stream
import           Text
import           TextParser

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

data OptionInfo = OptionInfo
  { optFlags :: NonEmpty Flag -- | A list of flags that trigger this option.
  , optHelp  :: Text -- | A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

data CommandInfo = CommandInfo
  { cmdNames :: NonEmpty Text
  , cmdHelp  :: Text -- | A description displayed in help output.
  } deriving (Eq, Ord, Show)

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

-- | A parser scheme for Unix-style CLI arguments.
data UnixScheme r
  = Parameter (TextParser r) -- ^ A standard freeform parameter
  | Command CommandInfo (ParseTree UnixScheme r) -- ^ A subcommand with its own parse tree
  | Option OptionInfo Bool (ParseTree SubScheme r) -- ^ A named option that
                                                   -- might support suboptions
  deriving (Functor)

instance Resolve UnixScheme where
  resolve (Parameter (TextParser hint _)) =
    throwError $ ExpectedError [render hint]
  resolve (Option info _ _) =
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
    -- arguments unless escaping is enabled.
    escaped <- getEscaped
    guard $ escaped || not ("-" `T.isPrefixOf` next)

    withContext (UnixArgument next) $
      pop_ *> runTextParser tp next

  activate (Option info compound subtree) = do
    -- Arguments should never be interpreted as options when escaped.
    getEscaped >>= guard . not

    (flag, mbound) <- peek >>= parseUnixOption
    guard $ flag `elem` optFlags info
    pop_

    let splitArgs s = if compound
                      then T.split (== ',') s
                      else [s]
        parseSubargs args =
          runTreeParser subtree
          StreamHandler
          { onSuccess = \state result -> pure (result, streamContent state)
          , onEmpty = flip throwWithContext "empty"
          , onFailure = throwWithContext
          , onHelpRequest = const requestHelp
          }
          (StreamState args [] (not compound))

    withContext (UnixOption flag mbound) $ do
      mnext <- peekMaybe
      case (mbound, mnext) of
        (Just argString, _) -> do
          (result, leftover) <- parseSubargs (splitArgs argString)
          forM_ leftover $ \arg ->
            throwError $ "unrecognized subargument: " <> render arg
          pure result
        (_, Just argString) -> do
          let args = splitArgs argString
          (result, leftover) <- parseSubargs args
          when (length args /= length leftover) $ do
            forM_ leftover $ \arg ->
              throwError $ "unrecognized subargument: " <> render arg
            pop_
          pure result
        _ -> do
          (result, _) <- parseSubargs []
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
  usageInfo (Option info _ subtree) =
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
