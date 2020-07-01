{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Pinch.Server where

import Pinch.Internal.Pinchable
import Pinch.Internal.Message
import Pinch.Internal.Exception
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text as T

newtype ThriftServer = ThriftServer { unThriftServer :: Message -> IO Message }



multiplexer :: [(EndpointName, ThriftServer)] -> ThriftServer
multiplexer endpoints = ThriftServer $ \msg -> do
  let (prefix, rem) = T.span (/= '.') (messageName msg)
  case prefix `HM.lookup` endpMap of
    _ | T.null rem -> throwAppEx msg $ ApplicationException "Invalid method name, expecting a dot." WrongMethodName
    Just srv -> unThriftServer srv $ msg { messageName = T.tail rem }
    Nothing -> throwAppEx msg $ ApplicationException ("No service with name " <> prefix <> " available.") UnknownMethod
    
  where
    endpMap = HM.fromList endpoints
    throwAppEx req ex = pure $ Message
      { messageName = messageName req
      , messageType = Exception
      , messageId = messageId req
      , messagePayload = pinch ex
      }




