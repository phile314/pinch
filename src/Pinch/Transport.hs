{-# LANGUAGE RankNTypes #-}

module Pinch.Transport
  ( Transport(..)
  , framedTransport
  , unframedTransport
  )where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.IORef
import Data.Serialize.Get
import Pinch.Internal.Builder as B

import System.IO

data Transport
  = Transport
  { writeMessage :: Builder -> IO ()
  , readMessage  :: forall a . Get a -> IO (Either String a)
  }



framedTransport :: Handle -> IO Transport
framedTransport h = pure $ Transport writeMsg readMsg where
  writeMsg msg = do
    BS.hPut h $ runBuilder $ int32BE (fromIntegral $ getSize msg)
    BS.hPut h $ runBuilder msg

  readMsg p = do
    szBs <- hGetExactly h 4
    let sz = fromIntegral <$> runGet getInt32be szBs
    case sz of
      Right x -> do
        msgBs <- hGetExactly h x
        pure $ runGet p msgBs
      Left s -> pure $ Left "Invalid frame size"


unframedTransport :: Handle -> IO Transport
unframedTransport h = do
  readBuffer <- newIORef mempty
  pure $ Transport writeMsg (readMsg readBuffer)
  where
    writeMsg msg = BS.hPut h $ runBuilder msg

    readMsg buf p = do
      bs <- readIORef buf
      bs' <- if BS.null bs then getSome else pure bs
      (leftOvers, r) <- runGetWith getSome p bs'
      writeIORef buf leftOvers
      pure r
    getSome = BS.hGetSome h 1024

hGetExactly :: Handle -> Int -> IO BS.ByteString
hGetExactly h n = do
  bs <- BS.hGet h n
  if BS.length bs < n then
    (bs <>) <$> hGetExactly h (n - BS.length bs)
  else
    pure bs

runGetWith :: IO BS.ByteString -> Get a -> BS.ByteString -> IO (BS.ByteString, Either String a)
runGetWith getBs p init = go (runGetPartial p init)
  where
    go r = case r of
      Fail err bs -> do
        putStrLn "Fail"
        pure (bs, Left err)
      Done r bs -> do
        putStrLn "Done"
        pure (bs, Right r)
      Partial cont -> do
        putStrLn "Partial"
        getBs >>= go . cont
