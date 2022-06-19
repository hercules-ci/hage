import qualified Codec.Binary.Bech32 as Bech32
import Control.Monad (replicateM)
import Crypto.Hage (AgeX25519Identity (AgeX25519Identity), encryptFile, generateFileKey, share, toRecipient)
import Crypto.Hage.Format (AgeParseError (NoIdentitiesFound), parseKeyFile, recipientToBech32, recipientsToHeader)
import qualified Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Either (fromLeft, fromRight)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Shelly
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), chooseBoundedIntegral, chooseInt, property)
import Test.QuickCheck.Gen (chooseUpTo, frequency)

withTmp :: (FilePath -> IO a) -> IO a
withTmp = withSystemTempDirectory "hage-test-tmp"

newtype Example a = Example a
  deriving newtype (Eq, Show)

instance Arbitrary (Example Text) where
  arbitrary = Example . T.pack <$> arbitrary

instance Arbitrary (Example ByteString) where
  arbitrary = do
    n <-
      frequency
        [ (10, chooseInt (0, 5)),
          (20, chooseInt (0, 100)),
          (1, chooseInt (0, 655360))
        ]
    Example . BS.pack <$> replicateM n (fromIntegral <$> chooseUpTo 0x100)

main :: IO ()
main =
  do
    hspec do
      describe "The age command, used as a reference in the test suite" do
        it "can generate keys, encrypt and decrypt" $ withTmp \tmp -> do
          pure () :: IO () -- shelly is too general
          shelly $ silently do
            let keyFile = T.pack (tmp </> ("key.txt" :: Text))
                recipientsFile = T.pack (tmp </> ("recip.txt" :: Text))
            out <- run "age-keygen" ["-o", keyFile]
            _ <- run "age-keygen" ["-y", "-o", recipientsFile, keyFile]
            let ciphFile = T.pack (tmp </> ("ciphertext" :: Text))
                plain = "It's a nice day today."
            setStdin plain
            _ <- run "age" ["--encrypt", "--recipients-file", recipientsFile, "-o", ciphFile]
            out <- run "age" ["--decrypt", "--identity", keyFile, ciphFile]
            liftIO do
              T.strip out `shouldBe` plain

      describe "Crypto.Hage.Format" do
        describe "parseKeyFile" do
          it "can parse a valid key" do
            let Right ((AgeX25519Identity sk) :| []) = parseKeyFile "AGE-SECRET-KEY-1UMQEQYKX6Y204ULF2HG46W98E256TQYYJJSHHYCX4Q2D72GHNAES7J8XVC"
            bs2 <- Data.ByteArray.copy sk mempty
            --             TOFU
            bs2 `shouldBe` ("\230\193\144\DC2\198\209\DC4\250\243\233U\209]8\167\202\169\165\128\132\148\161{\147\ACK\168\DC4\223)\ETB\159s" :: ByteString)

          it "can parse a valid key file" do
            let Right ((AgeX25519Identity sk) :| []) = parseKeyFile "# comment\nAGE-SECRET-KEY-1UMQEQYKX6Y204ULF2HG46W98E256TQYYJJSHHYCX4Q2D72GHNAES7J8XVC\n"
            bs2 <- Data.ByteArray.copy sk mempty
            --             TOFU
            bs2 `shouldBe` ("\230\193\144\DC2\198\209\DC4\250\243\233U\209]8\167\202\169\165\128\132\148\161{\147\ACK\168\DC4\223)\ETB\159s" :: ByteString)

      describe "Crypto.Hage" do
        describe "toRecipient" do
          it "can extract a recipient from an identity" do
            let Right (ident :| []) = parseKeyFile "# comment\nAGE-SECRET-KEY-1UMQEQYKX6Y204ULF2HG46W98E256TQYYJJSHHYCX4Q2D72GHNAES7J8XVC\n"
                recip = toRecipient ident
            -- constant from age execution
            recipientToBech32 recip `shouldBe` "age1p8l6mhqvl5zp04g8369nw7fdz2t9msnkpc3xuvz9gcapgs7a79dqcs3yp7"

        describe "share, encryptFile" do
          it "can generate keys, encrypt and be decrypted by age" $
            property $ \(Example plain) -> withTmp \tmp -> do
              shelly @IO $ silently do
                let keyFile = T.pack (tmp </> ("key.txt" :: Text))
                    recipientsFile = T.pack (tmp </> ("recip.txt" :: Text))
                out <- run "age-keygen" ["-o", keyFile]
                _ <- run "age-keygen" ["-y", "-o", recipientsFile, keyFile]
                let ciphFile = T.pack (tmp </> ("ciphertext" :: Text))

                liftIO do
                  contents <- BS.readFile (T.unpack keyFile) <&> TE.decodeUtf8
                  let Right (ident :| []) = parseKeyFile contents
                      recip = toRecipient ident

                  fileKey <- generateFileKey

                  stanza <- share fileKey recip

                  ciphertext <- encryptFile fileKey plain
                  let out = recipientsToHeader fileKey [stanza] <> ciphertext
                  BS.writeFile (T.unpack ciphFile) out
                  pure ()

                out <- runBS "age" ["--decrypt", "--identity", keyFile, ciphFile]
                liftIO do
                  out `shouldBe` plain

-- newline-safe
runBS :: FilePath -> [Text] -> Sh ByteString
runBS exe args = runHandle exe args (liftIO . BS.hGetContents)
