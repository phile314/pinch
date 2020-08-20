{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinch.Client
  ( EndpointName
  , Client
  , Multiplex(..)
  , ThriftCall(..)
  , ThriftError(..)
  , call
  , callOrThrow
  , simpleClient
  , ThriftResult(..)
  ) where

import Prelude

import Control.Exception
import Pinch.Internal.TType
import Pinch.Internal.Exception
import Pinch.Internal.Pinchable
import Pinch.Internal.Builder
import Pinch.Internal.Message
import Pinch.Transport
import Pinch.Protocol
import qualified Data.Text as T

data Client = Client
  { cTransportIn :: !Transport
  , cTransportOut :: !Transport
  , cProtocolIn :: !Protocol
  , cProtocolOut :: !Protocol
  , cMultiplex :: !Multiplex
  }

data Multiplex = Simplex | MultiplexTo !EndpointName

data ThriftCall a = ThriftCall
  { request :: !Message
  }

data ThriftError = ThriftError T.Text
  deriving (Show, Eq)

instance Exception ThriftError

call :: (Pinchable a, Tag a ~ TStruct) => Client -> ThriftCall a -> IO a
call (Client tIn tOut pIn pOut plex) c = do
  writeMessage tOut $ serializeMessage pOut $ mux plex (request c)
  reply <- readMessage tIn $ deserializeMessage' pIn
  case reply of
    Left err -> throwIO $ ThriftError $ "Could not read message: " <> T.pack err
    Right reply -> case messageType reply of
      Reply -> case runParser $ unpinch $ messagePayload reply of
        Right x -> pure x
        Left err -> do
          throwIO $ ThriftError $ "Could not parse reply payload: " <> T.pack err
      Exception -> case runParser $ unpinch $ messagePayload reply of
        Right (x :: ApplicationException) -> throwIO x
        Left err ->
          throwIO $ ThriftError $ "Could not parse application exception: " <> T.pack err
      t -> throwIO $ ThriftError $ "Expected reply or exception, got " <> (T.pack $ show t) <> "."

callOrThrow :: ThriftResult a => Client -> ThriftCall a -> IO (ResultType a)
callOrThrow client c = do
  r <- call client c
  case toEither r of
    Left (SomeException e) -> throwIO e
    Right x -> pure x


simpleClient :: Transport -> Protocol -> Multiplex -> Client
simpleClient t p plex = Client t t p p plex

class (Pinchable a, Tag a ~ TStruct) => ThriftResult a where
  type ResultType a
  toEither :: a -> Either SomeException (ResultType a)

instance ThriftResult () where
  type ResultType () = ()
  toEither _ = Right ()

mux :: Multiplex -> Message -> Message
mux m msg = case m of
  Simplex -> msg
  MultiplexTo srv -> msg { messageName = srv <> ":" <> messageName msg }
