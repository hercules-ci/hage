module Main where

import Control.Monad (join)
import Options.Applicative
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import System.IO (stderr, hPutStrLn)
import Crypto.Hage.Format (parseKeyFile, recipientToBech32)
import qualified Data.Text.Encoding as TE
import Data.Foldable (traverse_)
import Data.Function ((&))
import System.Exit (exitFailure)
import Crypto.Hage (toRecipient)

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
  pure do
    error "unimplemented: encrypt"
