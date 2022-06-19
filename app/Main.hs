{-# LANGUAGE ApplicativeDo #-}

module Main where

import Conduit (runConduit, runConduitRes, sinkFileBS, stdinC, yield, (.|))
import Control.Monad (join)
import Crypto.Hage (encryptFileConduit, generateFileKey, toRecipient)
import Crypto.Hage.Format (parseKeyFile, parseRecipientAddress, recipientToBech32, recipientsToHeader)
import qualified Crypto.Hage.Recipient.X25519 as X25519
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = join (execParser opts)

opts :: ParserInfo (IO ())
opts =
  info
    (sample <**> helper)
    (fullDesc <> progDesc "hage -- Haskell/age encryption tool")

sample :: Parser (IO ())
sample =
  subparser
    ( command "to-recipient-key" (info toRecipientKeyCommandParser (progDesc "Extract a recipient key from an identity"))
        <> command "encrypt" (info encryptCommandParser (progDesc "Encrypt data into an age file"))
        <> command "decrypt" (info decryptCommandParser (progDesc "Decrypt data from an age file"))
    )

toRecipientKeyCommandParser :: Parser (IO ())
toRecipientKeyCommandParser =
  pure do
    inBS <- BS.getContents
    keys <- case parseKeyFile (TE.decodeUtf8 inBS) of
      Left e -> do
        hPutStrLn stderr ("hage: " <> show e)
        exitFailure
      Right keys -> pure keys
    keys & traverse_ (T.putStrLn . recipientToBech32 . toRecipient)

decryptCommandParser :: Parser (IO ())
decryptCommandParser = do
  pure do
    error "unimplemented: decrypt"

encryptCommandParser :: Parser (IO ())
encryptCommandParser = do
  output <-
    strOption
      ( long "output"
          <> completer (bashCompleter "file")
      )

  recipients <-
    many
      ( option
          x25519recipientsReader
          ( long "recipient"
          )
      )

  pure do
    fileKey <- generateFileKey
    stanzas <- recipients & traverse (X25519.share fileKey)
    let header = recipientsToHeader fileKey stanzas
    encrypt <- encryptFileConduit fileKey
    runConduitRes do
      ( do
          yield header
          stdinC .| encrypt
        )
        .| sinkFileBS output

x25519recipientsReader :: ReadM X25519.AgeX25519RecipientAddress
x25519recipientsReader = eitherReader \s ->
  s & T.pack & parseRecipientAddress & \case
    Left e -> Left (show e)
    Right r -> Right r
