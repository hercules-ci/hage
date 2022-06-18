module Crypto.Hage where

import Crypto.PubKey.Curve25519 (SecretKey, toPublic, PublicKey)
import Crypto.ECC (Curve_X25519(Curve_X25519), encodePoint)
import Data.Data (Proxy(Proxy))
import Data.ByteString (ByteString)

newtype AgeIdentity = AgeIdentity SecretKey
newtype AgeRecipient = AgeRecipient PublicKey

toRecipient :: AgeIdentity -> AgeRecipient
toRecipient (AgeIdentity sk) = AgeRecipient (toPublic sk)
