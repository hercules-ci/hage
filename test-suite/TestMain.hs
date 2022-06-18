import qualified Codec.Binary.Bech32 as Bech32
import Crypto.Hage.Format (AgeParseError (NoIdentitiesFound), parseKeyFile, recipientToBech32)
import Data.Either (fromLeft, fromRight)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T
import Shelly
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (describe, hspec, it, shouldBe)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.ByteString as BS
import Crypto.Hage (AgeIdentity(AgeIdentity), toRecipient)
import qualified Data.ByteArray
import Data.ByteString (ByteString)

main :: IO ()
main =
  withSystemTempDirectory "hage-test-tmp" $ \tmp -> do
    hspec do
      describe "The age command, used as a reference in the test suite" do
        it "can generate keys, encrypt and decrypt" do
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
            let Right ((AgeIdentity sk) :| []) = parseKeyFile "AGE-SECRET-KEY-1UMQEQYKX6Y204ULF2HG46W98E256TQYYJJSHHYCX4Q2D72GHNAES7J8XVC"
            bs2 <- Data.ByteArray.copy sk mempty
            --             TOFU
            bs2 `shouldBe` ("\230\193\144\DC2\198\209\DC4\250\243\233U\209]8\167\202\169\165\128\132\148\161{\147\ACK\168\DC4\223)\ETB\159s" :: ByteString)

          it "can parse a valid key file" do
            let Right ((AgeIdentity sk) :| []) = parseKeyFile "# comment\nAGE-SECRET-KEY-1UMQEQYKX6Y204ULF2HG46W98E256TQYYJJSHHYCX4Q2D72GHNAES7J8XVC\n"
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
