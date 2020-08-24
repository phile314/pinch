{-# LANGUAGE RankNTypes #-}

module Pinch.Transport
  ( Transport(..)
  , framedTransport
  , unframedTransport
  )where

import Prelude

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.IORef
import Data.Serialize.Get
import Pinch.Internal.Builder as B

import System.IO

class Connection c where
  cGet :: c -> Int -> IO BS.ByteString
  cGetSome :: c -> Int -> IO BS.ByteString
  cPut :: c -> BS.ByteString -> IO ()

data Transport
  = Transport
  { writeMessage :: Builder -> IO ()
  , readMessage  :: forall a . Get a -> IO (Either String a)
  }

instance Connection Handle where
  cPut = BS.hPut
  cGet = BS.hGet
  cGetSome = BS.hGetSome

framedTransport :: Connection c => c -> IO Transport
framedTransport c = pure $ Transport writeMsg readMsg where
  writeMsg msg = do
    cPut c $ runBuilder $ int32BE (fromIntegral $ getSize msg)
    cPut c $ runBuilder msg

  readMsg p = do
    szBs <- cGetExactly c 4
    let sz = fromIntegral <$> runGet getInt32be szBs
    case sz of
      Right x -> do
        msgBs <- cGetExactly c x
        pure $ runGet p msgBs
      Left s -> pure $ Left "Invalid frame size"

unframedTransport :: Connection c => c -> IO Transport
unframedTransport c = do
  readBuffer <- newIORef mempty
  pure $ Transport writeMsg (readMsg readBuffer)
  where
    writeMsg msg = cPut c $ runBuilder msg

    readMsg buf p = do
      bs <- readIORef buf
      bs' <- if BS.null bs then getSome else pure bs
      (leftOvers, r) <- runGetWith getSome p bs'
      writeIORef buf leftOvers
      pure r
    getSome = cGetSome c 1024

cGetExactly :: Connection c => c -> Int -> IO BS.ByteString
cGetExactly c n = do
  bs <- cGet c n
  if BS.length bs < n then
    (bs <>) <$> cGetExactly c (n - BS.length bs)
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
