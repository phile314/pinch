{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Pinch.Internal.RPC
  ( EndpointName(..)
  , ThriftResult(..)
  , wrapExceptions
  , Wrapper(..)
  , Unit(..)
  ) where

import Data.Hashable   (Hashable)
import Data.Text       (Text)
import Control.Exception
import qualified Data.HashMap.Strict     as HM
import Data.String
import qualified Data.Text          as T
import Data.Typeable
import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import Pinch.Internal.Value

newtype EndpointName = EndpointName Text
  deriving (Typeable, Eq, Hashable)

instance IsString EndpointName where
  fromString = EndpointName . T.pack


class (Pinchable a, Tag a ~ TStruct) => ThriftResult a where
  type ResultType a
  toEither :: a -> Either SomeException (ResultType a)
  wrapException :: SomeException -> Maybe a
  success :: ResultType a -> a

instance ThriftResult Unit where
  type ResultType Unit = ()
  toEither Unit = Right ()
  wrapException _ = Nothing
  success () = Unit

data Wrapper a = forall e . Exception e => Wrapper (e -> a)

wrapExceptions :: [Wrapper a] -> SomeException -> Maybe a
wrapExceptions hs e = foldr tryHandler Nothing hs
  where
    tryHandler (Wrapper w) res =
      case fromException e of
        Just e' -> Just $ w e'
        Nothing -> res

data Unit = Unit

instance Pinchable Unit where
    type Tag Unit = TStruct
    pinch Unit = VStruct mempty
    unpinch (VStruct xs) | HM.null xs = pure Unit
    unpinch x = fail $ "Failed to read void success. Got " ++ show x

