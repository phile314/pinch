{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Pinch.Server
  ( ThriftServer
  , InvalidDataError (..)
  , Channel (..)
  , Context
  , ContextItem (..)
  , addToContext
  , lookupInContext

  , simpleServer
  , threadedServer
  , multiplexer
  ) where

import Prelude

import Pinch.Internal.Pinchable
import Pinch.Internal.Message
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
  let (prefix, rem) = T.span (/= '.') (messageName msg)
  let prefix' = EndpointName prefix
  let ctx' = addToContext prefix' ctx
  case prefix' `HM.lookup` endpMap of
    _ | T.null rem -> pure $ msgAppEx msg $ ApplicationException "Invalid method name, expecting a dot." WrongMethodName
    Just srv -> do
      reply <- unThriftServer srv ctx' $ msg { messageName = T.tail rem }
      pure $ reply { messageName = prefix <> "." <> messageName reply }
    Nothing -> pure $ msgAppEx msg $ ApplicationException ("No service with name " <> prefix <> " available.") UnknownMethod
    
  where
    endpMap = HM.fromList endpoints

threadedServer
  :: IO h -- accept new connections
  -> (h -> IO (Context, Channel)) -- create channel
  -> (h -> IO ()) -- release 
  -> ThriftServer
  -> IO ()
threadedServer accept init close srv = forever $ do
  h <- accept
  forkFinally (init h >>= run) (\_ -> close h)
  where
    run (ctx, chan) = do
      msg <- readMessage (cTransportIn chan) $ deserializeMessage' (cProtocolIn chan)
      reply <- case msg of
        Left err -> throwIO $ InvalidDataError $ T.pack err
        Right call ->  case messageType call of
          Call -> unThriftServer srv ctx call
          t -> pure $ msgAppEx call $ ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
      writeMessage (cTransportOut chan) $ serializeMessage (cProtocolOut chan) reply
    
msgAppEx :: Message -> ApplicationException -> Message
msgAppEx req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
