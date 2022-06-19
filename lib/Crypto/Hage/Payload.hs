{-# LANGUAGE DataKinds #-}

module Crypto.Hage.Payload
  ( encryptFileConduit,
    encryptFile,
  )
where

import Conduit (ConduitT, await, awaitForever, chunksOfCE, runConduit, sinkList, yield, (.|))
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaP
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hage.FileKey (AgeFileKey (AgeFileKey))
import Crypto.Hash (SHA256)
import Crypto.KDF.HKDF (PRK)
import qualified Crypto.KDF.HKDF as HKDF
import Crypto.Random.Entropy (getEntropy)
import qualified Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Combinators (peek)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))

-- | Encrypt the payload in one go. Use 'encryptFileConduit'.
encryptFile :: AgeFileKey -> ByteString -> IO ByteString
encryptFile fk plain = do
  c <- encryptFileConduit fk
  runConduit (yield plain .| c .| sinkList) <&> BS.concat

-- | A conduit that encrypts the payload.
encryptFileConduit :: Monad m => AgeFileKey -> IO (ConduitT ByteString ByteString m ())
encryptFileConduit fileKey = do
  nonce <- getEntropy 16
  let streamKey = toStreamKey fileKey nonce

      encryptBlocks = do
        awaitForever \(n, (isLast, plainblock)) -> do
          let result = do
                nonce2 <-
                  ChaP.nonce12 . BL.toStrict . BB.toLazyByteString $
                    "\0\0\0" <> BB.word64BE n <> if isLast then BB.word8 1 else BB.word8 0
                st1 <- ChaP.initialize streamKey nonce2
                let st2 = ChaP.finalizeAAD st1
                    (out, st3) = ChaP.encrypt plainblock st2
                    auth = ChaP.finalize st3
                pure do
                  yield (Data.ByteArray.convert out)
                  yield (Data.ByteArray.convert auth)
          case result of
            CryptoFailed e -> error ("Crypto.Hage.encryptFileConduit failed unexpectedly: " <> show e)
            CryptoPassed c -> c

  pure do
    yield nonce
    chunksOfCE (2 ^ (16 :: Int)) .| atLeastOneBlock .| withIsLast .| withCounter 0 .| encryptBlocks

toStreamKey :: AgeFileKey -> ByteString -> ByteString
toStreamKey (AgeFileKey fileKey) nonce =
  let prk :: PRK SHA256
      prk = HKDF.extract nonce fileKey
   in HKDF.expand prk payloadLabel 32

-- | Produce an empty block when no blocks are produced.
-- Identity conduit otherwise (pass through).
atLeastOneBlock :: Monad m => ConduitT ByteString ByteString m ()
atLeastOneBlock =
  await >>= \case
    Nothing -> yield mempty
    Just a -> do
      yield a
      awaitForever yield

-- | Count with @(+ 1)@
withCounter :: (Num n, Monad m) => n -> ConduitT a (n, a) m ()
withCounter i =
  await >>= traverse_ \a -> do
    yield (i, a)
    withCounter (i + 1)

-- | Add a boolean indicating whether the item is the last one to be produced.
withIsLast :: (Monad m) => ConduitT a (Bool, a) m ()
withIsLast = awaitForever \v ->
  peek >>= \case
    Nothing -> yield (True, v)
    Just _ -> yield (False, v)

payloadLabel :: ByteString
payloadLabel = "payload"
