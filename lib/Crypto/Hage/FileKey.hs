{-# LANGUAGE DataKinds #-}

module Crypto.Hage.FileKey
  ( AgeFileKey (..),
    generateFileKey,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray.Sized (SizedByteArray, unsafeSizedByteArray)
import Data.ByteString (ByteString)

newtype AgeFileKey = AgeFileKey (SizedByteArray 16 ByteString)

generateFileKey :: IO AgeFileKey
generateFileKey =
  -- unsafeSizedByteArray is only "unsafe" because it can throw if used incorrectly.
  AgeFileKey . unsafeSizedByteArray <$> getEntropy 16
