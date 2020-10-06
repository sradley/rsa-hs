{-# LANGUAGE ViewPatterns #-}
module Rsa
( Key (..)
, encrypt
, decrypt
, generate
, extract
) where

import Rsa.Random
import Rsa.Prime
import Rsa.Util

import qualified Data.ByteString.Lazy as BS
import           System.Random        (StdGen, getStdGen, randomR)

-- |Stores the components of an RSA public/private key.
data Key = 
    Public Integer Integer |
    Private Integer Integer Integer
    deriving (Show)

-- |Generates an n-bit RSA key.
generate :: Int -> IO Key
generate x = do p <- genPrime x
                q <- genPrime x 
                let d = maybe 0 (\d' -> d') $ modInv 65537 (phi p q)
                pure $ Private d (n p q) 65537 
    where n p q   = p*q
          phi p q = lcm (p - 1) (q - 1)

-- |Extracts a public key from a private key.
extract :: Key -> Key
extract (Public n e)    = Public n e
extract (Private _ n e) = Public n e

-- |Encrypt under RSA given a public or private key.
encrypt :: Key -> Integer -> Integer
encrypt (Public n e) x    = powMod x e n
encrypt (Private _ n e) x = powMod x e n 

-- |Decrypt under RSA given a private key.
decrypt :: Key -> Integer -> Maybe Integer
decrypt (Public _ _) _    = Nothing
decrypt (Private d n _) x = Just (powMod x d n)

