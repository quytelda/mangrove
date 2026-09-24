{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module Arbitrary
  ( ArgList(..)
  , Name(..)
  ) where

import           Data.Char
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.Random
import           Test.QuickCheck           hiding (Result (..))
import           Test.QuickCheck.Gen       (Gen (..))
import           Test.QuickCheck.Instances ()

import           Mangrove
import           Mangrove.ParseTree
import           Mangrove.Render
import qualified Mangrove.Scheme.Sub       as Sub
import           Mangrove.Scheme.Unix
import qualified Mangrove.Scheme.Unix      as Unix
import           Mangrove.Stream
import           Mangrove.TextParser
import           Mangrove.Unix

--------------------------------------------------------------------------------
-- Arbitrary Name Generator

randomNameChar :: RandomGen g => g -> (Char, g)
randomNameChar gen = (chr (n + offset), gen')
  where
    (n, gen') = uniformR (0, 62) gen
    offset
      | n >= 0  && n < 10 = 48
      | n >= 10 && n < 36 = 55
      | n >= 36 && n < 62 = 61
      -- The only remaining case is n == 62.
      | otherwise = 33

randomNameText :: RandomGen g => g -> Int -> Text
randomNameText gen n = T.unfoldrN n (Just . randomNameChar) gen

genNameChar :: Gen Char
genNameChar = MkGen $ const . fst . randomNameChar

genNameText :: Gen Text
genNameText = MkGen randomNameText `suchThat` (not . T.null)

-- | newtype wrapper for 'Text' that holds results from 'genNameText'
newtype Name = Name { getName :: Text }
  deriving (Eq, Show)

getNames :: Functor f => f Name -> f Text
getNames = fmap getName

instance Arbitrary Name where
  arbitrary = Name <$> genNameText

--------------------------------------------------------------------------------
-- Text Parsers

genTextParser :: Arbitrary a => Gen (TextParser a)
genTextParser = TextParser <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (TextParser a) where
  arbitrary = genTextParser

--------------------------------------------------------------------------------
-- Generic ParseTrees

genParser
  :: (Arbitrary a, CoArbitrary a, Scheme s)
  => Gen (s a)
  -> Gen (ParseTree s a)
genParser genScheme = sized $ \n -> oneof $
  if n <= 0
  then [ pure EmptyNode, ValueNode <$> arbitrary ]
  else [ pure EmptyNode
       , ValueNode <$> arbitrary
       , ParseNode <$> genScheme
       , ProdNode <$> arbitrary
                  <*> genParser genScheme
                  <*> genParser genScheme
       , SumNode <$> genParser genScheme
                 <*> genParser genScheme
         -- `ManyNode` can only give us a `ParseTree s [a]`, so we
         -- have to wrap it in order to make the types match. This
         -- also has the nice side-effect of ensuring we only generate
         -- trees whose structure is preserved under fmap.
       , liftA2 fmap arbitrary
         $ ManyNode <$> arbitrary
                    <*> genParser genScheme
       ]

--------------------------------------------------------------------------------
-- SubScheme Parsers

instance Arbitrary (Token SubScheme) where
  arbitrary = oneof
    [ SubAssoc <$> genNameText <*> arbitrary
    , SubArgument <$> arbitrary
    ]

instance CoArbitrary (Token SubScheme)

genSubScheme :: Arbitrary a => Gen (SubScheme a)
genSubScheme = oneof
  [ Sub.Parameter <$> arbitrary
  , Sub.Option <$> arbitrary <*> arbitrary
  ]

instance (Arbitrary a, CoArbitrary a) => Arbitrary (ParseTree SubScheme a) where
  arbitrary = genParser genSubScheme

--------------------------------------------------------------------------------
-- UnixScheme Parsers

instance Arbitrary Flag where
  arbitrary = oneof
    [ LongFlag <$> genNameText
    , ShortFlag <$> genNameChar
    ]

instance CoArbitrary Flag

instance Arbitrary Unix.OptionInfo where
  arbitrary = OptionInfo <$> arbitrary <*> arbitrary

instance Arbitrary Unix.CommandInfo where
  arbitrary = CommandInfo <$> fmap getNames arbitrary <*> arbitrary

instance Arbitrary (Token UnixScheme) where
  arbitrary = oneof
    [ UnixArgument <$> arbitrary
    , UnixCommand <$> genNameText
    , UnixOption <$> arbitrary <*> arbitrary
    ]

instance CoArbitrary (Token UnixScheme)

instance Arbitrary UnixRequest where
  arbitrary = elements
    [ HelpRequest []
    , VersionRequest
    ]

instance CoArbitrary UnixRequest

genUnixScheme :: (Arbitrary a, CoArbitrary a) => Gen (UnixScheme a)
genUnixScheme = oneof
  [ Unix.Parameter <$> arbitrary
  , Unix.Option <$> arbitrary <*> arbitrary
  , Unix.Command <$> arbitrary <*> arbitrary
  , Unix.RequestOption <$> arbitrary <*> arbitrary
  ]

instance (Arbitrary a, CoArbitrary a) => Arbitrary (ParseTree UnixScheme a) where
  arbitrary = genParser genUnixScheme

--------------------------------------------------------------------------------
-- StreamParsers

genUnixArgument :: Gen Text
genUnixArgument = renderText <$> arbitrary @(Token UnixScheme)

genUnixArgs :: Gen [Text]
genUnixArgs = sized $ \n -> vectorOf n genUnixArgument

newtype ArgList = ArgList { getArgs :: [Text] }
  deriving (Show)

instance Arbitrary ArgList where
  arbitrary = ArgList <$> genUnixArgs

instance Arbitrary (StreamState (Token UnixScheme)) where
  arbitrary = StreamState <$> genUnixArgs <*> arbitrary <*> arbitrary

instance CoArbitrary (StreamState (Token UnixScheme))
