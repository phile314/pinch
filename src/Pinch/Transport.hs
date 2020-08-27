{-# LANGUAGE RankNTypes #-}

module Pinch.Transport
  ( Transport(..)
  , framedTransport
  , unframedTransport
  , Connection(..)
  , ReadResult(..)
  )where

import Prelude

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.IORef
import Data.Serialize.Get
import Pinch.Internal.Builder as B

import Network.Socket (Socket)
import Network.Socket.ByteString

import System.IO

class Connection c where
  -- Gets up to n bytes. Returns an empty bytestring if EOF is reached.
  cGetSome :: c -> Int -> IO BS.ByteString
  -- Gets exactly n bytes. Returns an empty bytestring if EOF is reached.
  cGetExactly :: c -> Int -> IO BS.ByteString
  cGetExactly c n = B.runBuilder <$> go n mempty
    where
      go :: Int -> B.Builder -> IO B.Builder
      go n b = do
        bs <- cGetSome c n
        let b' = b <> B.byteString bs
        case BS.length bs of
          -- EOF, return what data we might have gotten so far
          0 -> pure b
          n' | n' < n -> go (n - n') b'
          _  | otherwise -> pure b'
  cPut :: c -> BS.ByteString -> IO ()

instance Connection Handle where
  cPut = BS.hPut
  cGetSome = BS.hGetSome

instance Connection Socket where
  cPut = sendAll
  cGetSome s n = recv s (min n 4096)

data ReadResult a
  = RRSuccess a
  | RRFailure String
  | RREOF

data Transport
  = Transport
  { writeMessage :: Builder -> IO ()
  , readMessage  :: forall a . Get a -> IO (ReadResult a)
  }

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
        if BS.length msgBs < x
          then
            -- less data has been returned than expected. This means we have reached EOF.
            pure $ RREOF
          else
            pure $ either RRFailure RRSuccess $ runGet p msgBs
      Left s -> pure $ RRFailure "Invalid frame size"

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
      pure $ r
    getSome = cGetSome c 1024

runGetWith :: IO BS.ByteString -> Get a -> BS.ByteString -> IO (BS.ByteString, ReadResult a)
runGetWith getBs p init = go (runGetPartial p init)
  where
    go r = case r of
      Fail err bs -> do
        pure (bs, RRFailure err)
      Done r bs -> do
        pure (bs, RRSuccess r)
      Partial cont -> do
        bs <- getBs
        if BS.null bs
          then
            -- EOF
            pure (bs, RREOF)
          else
            go $ cont bs
