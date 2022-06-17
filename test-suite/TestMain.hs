import Test.Hspec (hspec, describe, it, shouldBe)
import System.IO.Temp (withSystemTempDirectory)
import Shelly
import qualified Data.Text as T
import Data.Text (Text)

main :: IO ()
main =
  withSystemTempDirectory "hage-test-tmp" $ \tmp -> do
    hspec do
      describe "The age command, used as a reference in the test suite" do
        it "can generate keys, encrypt and decrypt" do
          pure () :: IO () -- shelly is too general
          shelly $ silently do
            let
              keyFile = T.pack (tmp </> ("key.txt" :: Text))
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
