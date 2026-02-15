{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Scheme.Cli
  ( Flag(..)
  , OptionInfo(..)
  , CommandInfo(..)
  , CliScheme(..)
  , Token(..)
  , optHead
  , cmdHead
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Functor
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NonEmpty
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB

import           ParseTree
import           Resolve
import           Scheme
import           Scheme.Sub
import           Stream
import           Text
import           TextParser

--------------------------------------------------------------------------------
-- User Interface Descriptions

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
  { optFlags :: NonEmpty Flag
  , optHelp  :: Text
  } deriving (Eq, Ord, Show)

-- | Get a representative flag for this option (e.g. the first one).
optHead :: OptionInfo -> Flag
optHead = NonEmpty.head . optFlags

data CommandInfo = CommandInfo
  { cmdNames :: NonEmpty Text
  , cmdHelp  :: Text
  } deriving (Eq, Ord, Show)

-- | Get a representative command name for this command (e.g. the
-- first one).
cmdHead :: CommandInfo -> Text
cmdHead = NonEmpty.head . cmdNames

-- | Parsers for top-level CLI arguments such as commands and options.
data CliScheme r
  = CliParameter (TextParser r)
  | CliOption OptionInfo Bool (ParseTree SubScheme r)
  | CliCommand CommandInfo (ParseTree CliScheme r)
  deriving (Functor)

instance Render (CliScheme r) where
  render = renderParser

instance HasValency CliScheme where
  valency (CliParameter _) = Just 1
  valency (CliOption _ _ subtree) =
    case valency subtree of
      Just n | n <= 0 -> Just 1
      _               -> Just 2
  valency (CliCommand _ subtree) = (+1) <$> valency subtree

instance Resolve CliScheme where
  resolve (CliParameter (TextParser hint _)) =
    throwError $ ExpectedError [TLB.fromText hint]
  resolve (CliOption info _ _) =
    throwError $ ExpectedError [render (optHead info)]
  resolve (CliCommand info _) =
    throwError $ ExpectedError [render (cmdHead info)]

parseLongOption :: Text -> Maybe (Text, Maybe Text)
parseLongOption (T.stripPrefix "--" -> Just s)
  | not (T.null s) =
    case keyEqualsValue s of
      Just (k, v) -> pure $ (k, Just v)
      Nothing     -> pure $ (s, Nothing)
parseLongOption _ = empty

parseShortOption :: Text -> Maybe (Char, Maybe Text)
parseShortOption = undefined

instance Scheme CliScheme where
  data Token CliScheme
    = LongOption Text (Maybe Text) -- ^ A long form flag (e.g. --option)
    | ShortOption Char (Maybe Text) -- ^ A short form flag (e.g. -c)
    | Argument Text -- ^ A freeform argument that is not an option
    | Command Text -- ^ A command
    deriving (Eq, Show)

  renderToken (LongOption k mv)  = "--" <> render k
                                   <> case mv of
                                        Nothing -> mempty
                                        Just v  -> "=" <> render v
  renderToken (ShortOption k mv) = "-" <> render k
                                   <> maybe mempty render mv
  renderToken (Argument s)       = render s
  renderToken (Command s)        = render s

  sepProd _ = " "
  sepSum _ = " | "

  renderParser (CliParameter tp) = render $ parserHint tp
  renderParser (CliOption info _ subtree) =
    let rep = optHead info
        separator = case rep of
                      LongFlag _  -> "="
                      ShortFlag _ -> " "
    in render rep
       <> if valencyIs (> 0) subtree
          then separator <> render subtree
          else mempty
  renderParser (CliCommand info subtree) = "{" <> render (cmdHead info) <> " "
                                           <> render subtree <> "}"

  activate (CliParameter tp) = do
    next <- peek
    escaped <- isEscaped
    guard $ escaped || not ("-" `T.isPrefixOf` next)

    withContext (Argument next) $
      pop_ *> runTextParser tp next
  activate (CliOption info compound subtree) = do
    escapeGuard

    token <- peek >>= \case
      (parseLongOption -> Just (k, mv))
        | LongFlag k `elem` optFlags info ->
          pure $ LongOption k mv
      (parseShortOption -> Just (k, mv))
        | ShortFlag k `elem` optFlags info ->
          pure $ ShortOption k mv
      _ -> empty
    pop_

    withContext token $ do
      -- Collect arguments for the subparser's stream from the next
      -- argument in the parent stream.
      args <- peekMaybe <&> \case
        Just s
          | not ("-" `T.isPrefixOf` s) ->
            if compound
            then T.split (== ',') s
            else [s]
        _                 -> []

      -- Evaluate the subparser in a new stream context.
      (result, leftovers) <- parseTree subtree StreamHandler
                             { onSuccess = \state result -> pure (result, streamContent state)
                             , onFailure = \_ -> throwError
                             , onEmpty = \_ -> throwError "empty"
                             , onHelpRequest = const requestHelp
                             }
                             (StreamState args [] (not compound))

      -- If the subparser consumed its input, we can safely remove it
      -- the from the parent stream. However, we cannot remove partially
      -- consumed input, so in that case we throw an error.
      when (length args /= length leftovers) $
        pop_ *> mapM_ (\arg -> throwError $ "unrecognized subargument: " <> render arg) leftovers

      pure result
  activate (CliCommand info subtree) = do
    escapeGuard

    next <- peek
    guard $ next `elem` cmdNames info
    pop_

    withContext (Command next) $
      satiate subtree
      >>= resolveLifted

-- addHelpOptions :: NonEmpty Flag -> ParseTree CliScheme r -> ParseTree CliScheme r
-- addHelpOptions flags tree = addHelp $ go tree
--   where
--     addHelp :: ParseTree CliScheme a -> ParseTree CliScheme a
--     addHelp _tree = ParseNode (CliHelp flags) <|> _tree

--     go :: ParseTree CliScheme a -> ParseTree CliScheme a
--     go (ParseNode (CliCommand info subtree)) =
--       ParseNode $ CliCommand info $ addHelp $ go subtree
--     go (ProdNode f l r) = ProdNode f (go l) (go r)
--     go (SumNode l r) = SumNode (go l) (go r)
--     go (ManyNode require p) = ManyNode require (go p)
--     go node = node
