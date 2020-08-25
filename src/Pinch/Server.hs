{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Pinch.Server
  ( ThriftServer(..)
  , InvalidDataError (..)
  , Channel (..)
  , Context
  , ContextItem (..)
  , addToContext
  , lookupInContext

  , simpleServer
  , threadedServer
  , multiplexer
  , runConnection
  , onError

  , runServiceMethod
  , unknownMethodError
  ) where

import Prelude

import Pinch.Internal.Pinchable
import Pinch.Internal.Message
import Pinch.Internal.RPC
import Pinch.Internal.TType
import Pinch.Internal.Exception
import Pinch.Protocol
import Pinch.Transport
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text as T
import Data.Hashable
import Data.Dynamic
import Data.Typeable
import Data.Proxy

newtype ThriftServer = ThriftServer { unThriftServer :: Context -> Message -> IO Message }

data InvalidDataError = InvalidDataError T.Text
  deriving (Show, Eq)
instance Exception InvalidDataError

newtype Context = Context (HM.HashMap TypeRep Dynamic)
class Typeable a => ContextItem a where
instance ContextItem EndpointName

instance Semigroup Context where
  (Context a) <> (Context b) = Context $ a <> b
instance Monoid Context where
  mempty = Context mempty

data Channel = Channel
  { cTransportIn :: !Transport
  , cTransportOut :: !Transport
  , cProtocolIn :: !Protocol
  , cProtocolOut :: !Protocol
  }


addToContext :: forall i . ContextItem i => i -> Context -> Context
addToContext i (Context m) =
  Context $ HM.insert (typeOf i) (toDyn i) m

lookupInContext :: forall i . ContextItem i => Context -> Maybe i
lookupInContext (Context m) = do
  x <- HM.lookup (typeRep (Proxy :: Proxy i)) m
  case fromDynamic @i x of
    Nothing -> error "Impossible!"
    Just y -> pure y
  

simpleServer :: (Pinchable c, Pinchable r, Tag c ~ TStruct, Tag r ~ TStruct) => (Context -> T.Text -> c -> IO r) -> ThriftServer
simpleServer f = ThriftServer $ \ctx msg -> do
  case runParser $ unpinch $ messagePayload msg of
    Right args -> do
      ret <- f ctx (messageName msg) args
      pure $ Message
        { messageName = messageName msg
        , messageType = Reply
        , messageId   = messageId msg
        , messagePayload = pinch ret
        }
    Left err -> do
      pure $ msgAppEx msg $ ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError

multiplexer :: [(EndpointName, ThriftServer)] -> ThriftServer
multiplexer endpoints = ThriftServer $ \ctx msg -> do
  let (prefix, rem) = T.span (/= ':') (messageName msg)
  let prefix' = EndpointName prefix
  let ctx' = addToContext prefix' ctx
  case prefix' `HM.lookup` endpMap of
    _ | T.null rem -> pure $ msgAppEx msg $ ApplicationException "Invalid method name, expecting a dot." WrongMethodName
    Just srv -> do
      reply <- try $ unThriftServer srv ctx' $ msg { messageName = T.tail rem }
      case reply of
        Right reply -> pure $ reply { messageName = prefix <> "." <> messageName reply }
        Left (err :: SomeException) -> pure $ msgAppEx msg $ ApplicationException (T.pack $ show err) InternalError
    Nothing -> pure $ msgAppEx msg $ ApplicationException ("No service with name " <> prefix <> " available.") UnknownMethod
    
  where
    endpMap = HM.fromList endpoints

onError :: (SomeException -> IO ()) -> ThriftServer -> ThriftServer
onError f srv = ThriftServer $
  \ctx req -> unThriftServer srv ctx req `catch` (\e -> do
    f e
    throwIO e
  )




threadedServer
  :: IO h -- accept new connections
  -> (h -> IO (Context, Channel)) -- create channel
  -> (h -> IO ()) -- release 
  -> ThriftServer
  -> IO ()
threadedServer accept init close srv = forever $ do
  h <- accept
  forkFinally (init h >>= \(ctx, chan) -> runConnection ctx srv chan) (\_ -> close h)

runConnection :: Context -> ThriftServer -> Channel -> IO ()
runConnection ctx srv chan = forever $ do
  msg <- readMessage (cTransportIn chan) $ deserializeMessage' (cProtocolIn chan)
  reply <- case msg of
    Left err -> throwIO $ InvalidDataError $ T.pack err
    Right call ->  case messageType call of
      Call -> do
        r <- try $ unThriftServer srv ctx call
        case r of
          Left (e :: SomeException) -> pure $ msgAppEx call $
            ApplicationException ("Could not process request: " <> (T.pack $ show e)) InternalError
          Right x -> pure x
      t -> pure $ msgAppEx call $ ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
  writeMessage (cTransportOut chan) $ serializeMessage (cProtocolOut chan) reply

tryOrWrap :: ThriftResult a => IO a -> IO a
tryOrWrap m = do
  x <- try m
  case x of
    Right x -> pure x
    Left e -> case wrapException e of
      Nothing -> throwIO e
      Just x -> pure x

runServiceMethod :: (Tag a ~ TStruct, Pinchable a, ThriftResult b) => (a -> IO b) -> Message -> IO Message
runServiceMethod m req = do
  case runParser $ unpinch $ messagePayload req of
    Left err -> pure $ msgAppEx req $ ApplicationException ("Could not parse arguments: " <> T.pack err) InternalError
    Right args -> do
      r <- tryOrWrap (m args)
      pure $ Message
        { messageName = messageName req
        , messageType = Reply
        , messageId = messageId req
        , messagePayload = pinch r
        }

unknownMethodError :: Message -> Message
unknownMethodError req = msgAppEx req $
  ApplicationException ("Unknown method: " <> messageName req) UnknownMethod


msgAppEx :: Message -> ApplicationException -> Message
msgAppEx req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
