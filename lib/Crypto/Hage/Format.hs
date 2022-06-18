module Crypto.Hage.Format
  ( parseKeyFile,
    AgeParseError (..),
    Bech32.DecodingError (..),
    recipientToBech32,
  )
where

import Codec.Binary.Bech32 (dataPartFromBytes)
import qualified Codec.Binary.Bech32 as Bech32
import Control.Exception (throw)
import Control.Monad (when)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hage
import Crypto.PubKey.Curve25519 (secretKey)
import Data.Bifunctor (first)
import qualified Data.ByteArray
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List (zip)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Data.Either (fromRight)

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

parseKeyFile :: Text -> Either AgeParseError (NonEmpty AgeIdentity)
parseKeyFile contents =
  T.lines contents
    & zip [1 ..]
    & filter (\(n, ln) -> ln /= "" && not ("#" `T.isPrefixOf` ln))
    & traverse (\(n, ln) -> Bech32.decode ln & first (Bech32DecodingError n) >>= validateKey n)
    & ( >>=
          \case
            [] -> Left NoIdentitiesFound
            (x : xs) -> Right (x :| xs)
      )
  where
    validateKey :: Int -> (Bech32.HumanReadablePart, Bech32.DataPart) -> Either AgeParseError AgeIdentity
    validateKey n (hp, dp) = do
      when (Bech32.humanReadablePartToText hp /= "age-secret-key-") do
        Left (InvalidKey n "unknown type")
      dps <- Bech32.dataPartToBytes dp & maybe (Left (InvalidKey n "invalid data")) Right
      case secretKey dps of
        CryptoFailed e -> Left (InvalidKey n "invalid key size")
        CryptoPassed r -> Right (AgeIdentity r)

recipientToBech32 :: AgeRecipient -> Text
recipientToBech32 (AgeRecipient pk) =
  let pkBytes = unsafePerformIO (Data.ByteArray.copy pk mempty)
   in case Bech32.encode recipientHumanPart (dataPartFromBytes pkBytes) of
        Left err ->
          error ("recipientToBech32: unexpected encoding error" <> show err)
        Right r -> r

recipientHumanPart :: Bech32.HumanReadablePart
recipientHumanPart = Bech32.humanReadablePartFromText "age" & fromRight (error "\"age\" unexpectedly invalid")
