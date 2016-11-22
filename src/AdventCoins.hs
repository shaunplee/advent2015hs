module AdventCoins where

import           Crypto.Hash
import qualified Data.ByteString.Lazy      as LB
import           Data.ByteString.Lazy.UTF8 (fromString)

md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy

secret = "iwrupvqb"

mine :: String -> Integer
mine s = mineRecur (fromString s) 1 where
    mineRecur sec c =
        let try = LB.append sec (fromString $ show c)
        in case show $ md5 try of
            ('0':'0':'0':'0':'0':'0':_) -> c
            otherwise                   -> mineRecur sec (c + 1)
