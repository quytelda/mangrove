{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Mangrove.Test.Stream
  ( -- * Unix Stream Parser
    SP_Unix_T

    -- * Stream Proxy
  , StreamProxy(..)
  , SPState
  , SPResult(..)
  , toStreamParser
  , fromStreamParser

    -- * Unix Stream Parser Wrapper
  , SP_Unix(..)
  , runSPU
  ) where

import           Control.Monad
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TLB
import           GHC.Generics
import           Test.QuickCheck           hiding (Result (..))
import           Test.QuickCheck.Instances ()

import           Mangrove.Scheme
import           Mangrove.Stream
import           Mangrove.Token
import           Mangrove.Unix

import           Arbitrary                 ()

type SPState = StreamState (Token UnixScheme)

data SPResult a
  = SPSuccess a
  | SPEmpty
  | SPFailure Text
  | SPRequest UnixRequest
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

toStreamParser :: StreamProxy a -> SP_Unix_T a
toStreamParser prox = StreamParser $ \handler state ->
  let (result, state') = runSP prox state
  in case result of
       SPSuccess a       -> onSuccess handler state' a
       SPEmpty           -> onEmpty handler state'
       SPFailure err     -> onFailure handler state' (TLB.fromText err)
       SPRequest reqType -> onRequest handler state' reqType

sinkResult :: StreamHandler (Request UnixScheme) (Token UnixScheme) a (SPResult a, SPState)
sinkResult = StreamHandler
  { onSuccess = \state' a -> (SPSuccess a, state')
  , onEmpty = \state' -> (SPEmpty, state')
  , onFailure = \state' err -> (SPFailure (TL.toStrict $ TLB.toLazyText err), state')
  , onRequest = \state' req -> (SPRequest req, state')
  }

type SP_Unix_T a = StreamParser (Request UnixScheme) (Token UnixScheme) a

fromStreamParser :: SP_Unix_T a -> StreamProxy a
fromStreamParser parser = SP $ runStreamParser parser sinkResult

genStreamParser :: Arbitrary a => Gen (SP_Unix_T a)
genStreamParser = toStreamParser <$> genSP

newtype SP_Unix a = SP_Unix
  { getSPU :: SP_Unix_T a }

instance Show (SP_Unix a) where
  show _ = "(*)"

instance Arbitrary a => Arbitrary (SP_Unix a) where
  arbitrary = SP_Unix <$> genStreamParser

--------------------------------------------------------------------------------
-- Stream Helper

runSPU
  :: SP_Unix_T a
  -> StreamState (Token UnixScheme)
  -> (SPResult a, StreamState (Token UnixScheme))
runSPU parser = runStreamParser parser sinkResult
