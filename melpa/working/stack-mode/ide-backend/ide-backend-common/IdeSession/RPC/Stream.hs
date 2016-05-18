-- | Wrapper around binary
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module IdeSession.RPC.Stream (
    Stream
  , newStream
  , nextInStream
  ) where

import Prelude hiding (take)
import System.IO (Handle, hIsEOF)
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.ByteString as BSS
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Data.Binary (Binary)
import qualified Data.Binary     as Binary
import qualified Data.Binary.Get as Binary

data Stream a where
  Stream :: Binary a => Handle -> IORef (Binary.Decoder a) -> Stream a

newStream :: Binary a => Handle -> IO (Stream a)
newStream h = do
  st <- newIORef $ Binary.runGetIncremental Binary.get
  return $ Stream h st

nextInStream :: forall a. Stream a -> IO a
nextInStream (Stream h st) = readIORef st >>= go False
  where
    go :: Bool -> Binary.Decoder a -> IO a
    go atEnd decoder = case decoder of
      Binary.Fail _ _ err -> do
        writeIORef st decoder
        throwUserError $
          if atEnd
            then "IdeSession.RPC.Stream ended, causing: " ++ err
            else "IdeSession.RPC.Stream decode failure: " ++ err
      Binary.Partial k -> do
        eof <- hIsEOF h
        -- For some reason doing this check helps avoiding race conditions in
        -- in various scenarios when the server is force terminated.
        -- E.g., the tests in SessionRestart.hs
        if eof then throwUserError "IdeSession.RPC.Stream input handle closed unexpectedly"
        else do
          mchunk <- Ex.try $ BSS.hGetSome h BSL.defaultChunkSize
          case mchunk of
            Left (ex :: Ex.SomeException) -> do
              writeIORef st decoder
              -- wrapping with a user error so that it's easier to catch later on
              throwUserError $ "IdeSession.RPC.Stream ended unexpectedly, causing: " ++ show ex
            Right chunk | BSS.null chunk -> go True . k $ Nothing
                        | otherwise      -> go False . k $ Just chunk
      Binary.Done unused _numConsumed a -> do
        writeIORef st $ contDecoder unused
        return a

    contDecoder :: BSS.ByteString -> Binary.Decoder a
    contDecoder = Binary.pushChunk (Binary.runGetIncremental Binary.get)

    throwUserError = Ex.throwIO . userError
