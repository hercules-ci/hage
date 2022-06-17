module Main where

import Control.Monad (join)
import Options.Applicative

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
    ( command "encrypt" (info encryptCommandParser (progDesc "Encrypt data into an age file"))
        <> command "decrypt" (info decryptCommandParser (progDesc "Decrypt data from an age file"))
    )

decryptCommandParser :: Parser (IO ())
decryptCommandParser = do
  pure do
    error "unimplemented: decrypt"

encryptCommandParser :: Parser (IO ())
encryptCommandParser = do
  pure do
    error "unimplemented: encrypt"
