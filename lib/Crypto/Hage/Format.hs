module Crypto.Hage.Format
  ( parseKeyFile,
    AgeParseError (..),
    Bech32.DecodingError (..),
    recipientToBech32,
    recipientsToHeader,
    parseRecipientAddress,
  )
where

import Codec.Binary.Bech32 (dataPartFromBytes)
import qualified Codec.Binary.Bech32 as Bech32
import Control.Monad (when)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hage
import Crypto.Hash (SHA256)
import qualified Crypto.KDF.HKDF as HKDF
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.PubKey.Curve25519 (secretKey, publicKey)
import Data.Bifunctor (first)
import qualified Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

data AgeParseError
  = Bech32DecodingError
      Int
      -- ^ Line number
      Bech32.DecodingError
  | InvalidKey
      Int
      -- ^ Line number
      Text
      -- ^ Message (safe)
  | NoIdentitiesFound
  deriving (Eq, Show)

parseKeyFile :: Text -> Either AgeParseError (NonEmpty AgeX25519Identity)
parseKeyFile contents =
  T.lines contents
    & zip [1 ..]
    & filter (\(_n, ln) -> ln /= "" && not ("#" `T.isPrefixOf` ln))
    & traverse (\(n, ln) -> Bech32.decode ln & first (Bech32DecodingError n) >>= validateKey n)
    & ( >>=
          \case
            [] -> Left NoIdentitiesFound
            (x : xs) -> Right (x :| xs)
      )
  where
    validateKey :: Int -> (Bech32.HumanReadablePart, Bech32.DataPart) -> Either AgeParseError AgeX25519Identity
    validateKey n (hp, dp) = do
      when (Bech32.humanReadablePartToText hp /= "age-secret-key-") do
        Left (InvalidKey n "unknown type")
      dps <- Bech32.dataPartToBytes dp & maybe (Left (InvalidKey n "invalid data")) Right
      case secretKey dps of
        CryptoFailed _e -> Left (InvalidKey n "invalid key size")
        CryptoPassed r -> Right (AgeX25519Identity r)

recipientToBech32 :: AgeX25519RecipientAddress -> Text
recipientToBech32 (AgeX25519RecipientAddress pk) =
  let pkBytes = unsafePerformIO (Data.ByteArray.copy pk mempty)
   in case Bech32.encode recipientHumanPart (dataPartFromBytes pkBytes) of
        Left err ->
          error ("recipientToBech32: unexpected encoding error" <> show err)
        Right r -> r

parseRecipientAddress :: Text -> Either Text AgeX25519RecipientAddress
parseRecipientAddress t = do
  (hp, dp) <- Bech32.decode t & first (T.pack . show)
  when (Bech32.humanReadablePartToText hp /= "age") do
    Left "Recipient is not an age X25519 address, because it does not start with age1."
  dps <- Bech32.dataPartToBytes dp & maybe (Left "Key data does not represent a valid X25519 key") Right
  case publicKey dps of
        CryptoFailed _e -> Left "Key size not valid for X25519 key"
        CryptoPassed r -> Right (AgeX25519RecipientAddress r)

recipientHumanPart :: Bech32.HumanReadablePart
recipientHumanPart = Bech32.humanReadablePartFromText "age" & fromRight (error "\"age\" unexpectedly invalid")

recipientToHeaderPart :: AgeX25519RecipientStanza -> ByteString
recipientToHeaderPart stanza =
  let hd = pureCopy hk
      hk = headerKey stanza
   in "-> X25519 " <> encodeBase64Unpadded hd <> "\n"
        <> encodeBase64Unpadded (body stanza)
        <> "\n"

pureCopy :: (Data.ByteArray.ByteArray a, Data.ByteArray.ByteArrayAccess bs1) => bs1 -> a
pureCopy x = unsafePerformIO (Data.ByteArray.copy x mempty)

encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = C8.takeWhile (/= '=') . B64.encode

recipientsToHeader :: AgeFileKey -> [AgeX25519RecipientStanza] -> ByteString
recipientsToHeader (AgeFileKey fileKey) recips =
  let headerWithoutMAC =
        "age-encryption.org/v1\n"
          <> BS.concat (map recipientToHeaderPart recips)
          <> "---"
      salt, info, hmacKey :: ByteString
      salt = ""
      info = "header"
      prk :: HKDF.PRK SHA256
      prk = HKDF.extract salt fileKey
      hmacKey = HKDF.expand prk info 32
      hmacValue :: HMAC SHA256
      hmacValue = hmac hmacKey headerWithoutMAC
   in headerWithoutMAC <> " " <> encodeBase64Unpadded (pureCopy (hmacGetDigest hmacValue)) <> "\n"
