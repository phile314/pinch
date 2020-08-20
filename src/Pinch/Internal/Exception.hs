{-# LANGUAGE TypeApplications, TypeFamilies, ScopedTypeVariables #-}
module Pinch.Internal.Exception where

import Prelude

import Data.Int
import Data.Typeable
import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import qualified Data.Text as T
import Control.Exception

data ApplicationException
  = ApplicationException
  { appExMessage :: T.Text
  , appExType :: ExceptionType
  }
  deriving (Show, Eq, Typeable)

instance Exception ApplicationException

instance Pinchable ApplicationException where
  type Tag ApplicationException = TStruct

  pinch p = struct
    [ 1 .= appExMessage p
    , 2 .= appExType p
    ]

  unpinch value = ApplicationException
    <$> value .: 1
    <*> value .: 2

data ExceptionType
  = Unknown
  | UnknownMethod
  | InvalidMessageType
  | WrongMethodName
  | BadSequenceId
  | MissingResult
  | InternalError
  | ProtocolError
  | InvalidTransform
  | InvalidProtocol
  | UnsupportedClientType
  deriving (Show, Eq, Enum, Bounded)

instance Pinchable ExceptionType where
  type Tag ExceptionType = TEnum

  pinch t = pinch ((fromIntegral $ fromEnum t) :: Int32)

  unpinch v = do
    value <- (fromIntegral :: Int32 -> Int) <$> unpinch v
    if (fromEnum $ minBound @ExceptionType) <= value && value <= (fromEnum $ maxBound @ExceptionType)
      then pure $ toEnum $ fromIntegral value
      else fail $ "Unknown application exception type: " ++ show value
