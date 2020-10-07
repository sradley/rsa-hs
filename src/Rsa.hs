{-# LANGUAGE ViewPatterns #-}
module Rsa
( Key (..)
, encrypt
, decrypt
, genKey
, extKey
) where

import Rsa.Prime
import Rsa.Util

-- |Stores the components of an RSA public/private key.
data Key = 
    Public Integer Integer |
    Private Integer Integer Integer
    deriving (Show)

-- |Generates an n-bit RSA key.
genKey :: Int -> IO Key
genKey x = (,) <$> genPrime x
               <*> genPrime x >>= pure . (\pq -> Private (d pq) (n pq) 65537)
    where n (p, q)   = p*q
          phi (p, q) = lcm (p - 1) (q - 1)
          d (p, q)   = maybe 0 (\y -> y) $ modInv 65537 (phi (p, q))

-- |Extracts a public key from a private key.
extKey :: Key -> Key
extKey (Public n e)    = Public n e
extKey (Private _ n e) = Public n e

-- |Encrypt under RSA given a public or private key.
encrypt :: Key -> Integer -> Integer
encrypt (Public n e) x    = powMod x e n
encrypt (Private _ n e) x = powMod x e n 

-- |Decrypt under RSA given a private key.
decrypt :: Key -> Integer -> Maybe Integer
decrypt (Public _ _) _    = Nothing
decrypt (Private d n _) x = Just (powMod x d n)

