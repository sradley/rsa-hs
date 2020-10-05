{-# LANGUAGE ViewPatterns #-}
module Rsa
( Key (..)
, encrypt
, decrypt
, generate
, extract
) where

import qualified Data.ByteString.Lazy as BS
import           System.Random        (StdGen, getStdGen, randomR)
import           Data.Bits

-- |Stores the components of an RSA public/private key.
data Key = 
    Public Integer Integer |
    Private Integer Integer Integer
    deriving (Show)

-- |Generates an n-bit RSA key.
generate :: Int -> IO Key
generate x = do p <- genPrime (x `div` 8)
                q <- genPrime (x `div` 8)
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

powMod :: Integer -> Integer -> Integer -> Integer
powMod x 1 m = x `mod` m
powMod x p m 
    | even p    = let y = powMod x (p `div` 2) m
                  in (y * y) `mod` m
    | otherwise = let y = powMod x ((p - 1) `div` 2) m
                  in (x * y * y) `mod` m 

genPrime :: Int -> IO Integer
genPrime n = do x <- randInt n
                g <- getStdGen
                if (isPrime x 10 g) then pure x
                else genPrime n

isPrime :: Integer -> Int -> StdGen -> Bool
isPrime n k g 
    | n == 1 || n == 4 = False
    | n <= 3           = True
    | otherwise        = and $ [isPrime' d n g | _ <- [0..k]]
    where
        d = calcd (n - 1)
        calcd x | even x    = calcd (x `div` 2)
                | otherwise = x

isPrime' :: Integer -> Integer -> StdGen -> Bool
isPrime' d n g
    | let x = powMod a d n
      in x == 1 || x == n - 1 = True 
    | otherwise               = primeTest (powMod a d n) d n
    where
        (a, _) = randomR (2, (n - 2)) g

primeTest :: Integer -> Integer -> Integer -> Bool
primeTest x d n
    | d == n - 1 = False
    | x == n - 1 = True
    | otherwise  = let x' = (x * x) `mod` n
                       d' = d * 2
                   in primeTest x' d' n

randInt :: Int -> IO Integer
randInt n = randBytes n >>= pure . bytesToInt

randBytes :: Int -> IO [Integer]
randBytes n = readbytes >>= pure . mapToInt . (BS.take (fromIntegral n))
    where
        readbytes = BS.readFile "/dev/urandom"
        mapToInt xs = map fromIntegral $ BS.unpack xs

bytesToInt :: [Integer] -> Integer
bytesToInt x = bytesToInt' 1 $ setFirstBit x
    where 
        bytesToInt' :: Integer -> [Integer] -> Integer
        bytesToInt' _ (reverse -> [])   = 0
        bytesToInt' i (reverse -> b:bs) = (b ^ i) + bytesToInt' (i + 1) bs

setFirstBit :: [Integer] -> [Integer]
setFirstBit [] = []
setFirstBit (b:bs) = (b .|. 128):bs

modInv :: Integer -> Integer -> Maybe Integer
modInv a m = let modInv' (g, x, _) | g /= 1    = Nothing
                                   | otherwise = Just (x `mod` m)
             in modInv' $ egcd a m

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let out (g, y, x) = (g, x - (b `div` a) * y, y)
           in out $ egcd (b `mod` a) a

