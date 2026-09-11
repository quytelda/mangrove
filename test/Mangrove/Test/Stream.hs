{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Mangrove.Test.Stream where

import           Control.Monad
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TLB
import           GHC.Generics
import           Test.QuickCheck           hiding (Result (..))
import           Test.QuickCheck.Instances ()

import           Mangrove.Stream
import           Mangrove.Unix

import           Arbitrary                 ()

type SPState = StreamState UnixScheme

data SPResult a
  = SPSuccess a
  | SPEmpty
  | SPFailure Text
  | SPRequest RequestType
  deriving (Eq, Show, Functor, Generic)

instance Arbitrary a => Arbitrary (SPResult a) where
  arbitrary = oneof
    [ SPSuccess <$> arbitrary
    , pure SPEmpty
    , SPFailure <$> arbitrary
    , SPRequest <$> arbitrary
    ]

newtype StreamProxy a = SP { runSP :: SPState -> (SPResult a, SPState) }
  deriving (Functor, Generic)

genSP :: Arbitrary a => Gen (StreamProxy a)
genSP = SP <$> arbitrary

instance Applicative StreamProxy where
  pure a = SP $ \state -> (SPSuccess a, state)
  (<*>) = ap

instance Monad StreamProxy where
  return = pure
  ma >>= f = SP $ \state ->
    let (result, state') = runSP ma state
    in case result of
         SPSuccess a       -> runSP (f a) state'
         SPEmpty           -> (SPEmpty, state')
         SPFailure err     -> (SPFailure err, state')
         SPRequest reqType -> (SPRequest reqType, state')

toStreamParser :: StreamProxy a -> StreamParser UnixScheme a
toStreamParser prox = StreamParser $ \handler state ->
  let (result, state') = runSP prox state
  in case result of
       SPSuccess a       -> onSuccess handler state' a
       SPEmpty           -> onEmpty handler state'
       SPFailure err     -> onFailure handler state' (TLB.fromText err)
       SPRequest reqType ->
         let OnRequest _onRequest = onRequest handler
         in _onRequest state' reqType

sinkResult :: StreamHandler UnixScheme a (SPResult a, SPState)
sinkResult = StreamHandler
  { onSuccess = \state' a -> (SPSuccess a, state')
  , onEmpty = \state' -> (SPEmpty, state')
  , onFailure = \state' err -> (SPFailure (TL.toStrict $ TLB.toLazyText err), state')
  , onRequest = OnRequest $ \state' req -> (SPRequest req, state')
  }

fromStreamParser :: StreamParser UnixScheme a -> StreamProxy a
fromStreamParser parser = SP $ runStreamParser parser sinkResult

genStreamParser :: Arbitrary a => Gen (StreamParser UnixScheme a)
genStreamParser = toStreamParser <$> genSP

newtype SP_Unix a = SP_Unix { getSPU :: StreamParser UnixScheme a }

instance Show (SP_Unix a) where
  show _ = "(*)"

instance Arbitrary a => Arbitrary (SP_Unix a) where
  arbitrary = SP_Unix <$> genStreamParser

--------------------------------------------------------------------------------
-- Stream Helper

runSPU
  :: StreamParser UnixScheme a
  -> StreamState UnixScheme
  -> (SPResult a, StreamState UnixScheme)
runSPU parser = runStreamParser parser sinkResult
