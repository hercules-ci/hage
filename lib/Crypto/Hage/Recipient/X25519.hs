module Crypto.Hage.Recipient.X25519
  ( AgeX25519Identity (..),
    AgeX25519RecipientAddress (..),
    AgeX25519RecipientStanza (..),
    toRecipient,
    share,
  )
where

import Control.Monad (when)
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaP
import Crypto.ECC (Curve_X25519, curveGenerateScalar)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hage.FileKey (AgeFileKey (..))
import Crypto.Hash (SHA256)
import Crypto.KDF.HKDF (PRK)
import qualified Crypto.KDF.HKDF as HKDF
import Crypto.PubKey.Curve25519 (PublicKey, SecretKey, dh, toPublic)
import qualified Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Proxy (Proxy))

newtype AgeX25519Identity = AgeX25519Identity SecretKey

newtype AgeX25519RecipientAddress = AgeX25519RecipientAddress PublicKey

data AgeX25519RecipientStanza = AgeX25519RecipientStanza
  { headerKey :: PublicKey,
    body :: ByteString
  }

toRecipient :: AgeX25519Identity -> AgeX25519RecipientAddress
toRecipient (AgeX25519Identity sk) = AgeX25519RecipientAddress (toPublic sk)

share :: AgeFileKey -> AgeX25519RecipientAddress -> IO AgeX25519RecipientStanza
share (AgeFileKey fileKey) (AgeX25519RecipientAddress recipPubkey) = do
  ephemeral <- curveGenerateScalar (Proxy @Curve_X25519)

  let ourPubkey = toPublic ephemeral
      sharedSecret = dh recipPubkey ephemeral

  when (Data.ByteArray.all (== 0) sharedSecret) do
    error "Crypto.Hage.share: sharedSecret must not be all-zero"

  (salt :: ByteString) <-
    (<>) <$> Data.ByteArray.copy ourPubkey mempty
      <*> Data.ByteArray.copy recipPubkey mempty

  let prk :: PRK SHA256
      prk = HKDF.extract salt sharedSecret

      wrappingKey :: ByteString
      wrappingKey = HKDF.expand prk x25519Label 32

      encrypt = do
        nonce <- ChaP.nonce12 (BS.replicate 12 0)
        st1 <- ChaP.initialize wrappingKey nonce
        let st2 = ChaP.finalizeAAD st1
            (out, st3) = ChaP.encrypt (Data.ByteArray.convert fileKey :: ByteString) st2
            auth = ChaP.finalize st3
        pure $ Data.ByteArray.convert out <> Data.ByteArray.convert auth

  wrappedKey <- case encrypt of
    CryptoFailed e -> error ("Crypto.Hage.share failed unexpectedly: " <> show e)
    CryptoPassed r -> pure r

  pure AgeX25519RecipientStanza {headerKey = ourPubkey, body = wrappedKey}

x25519Label :: ByteString
x25519Label = "age-encryption.org/v1/X25519"
