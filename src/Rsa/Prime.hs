module Rsa.Prime
( genPrime
) where

import Rsa.Random
import Rsa.Util
import System.Random (StdGen, getStdGen, randomR)

-- |Generates a cryptographically secure prime number of size n-bits.
genPrime :: Int -> IO Integer 
genPrime n = (getCryptoGen n) >>= genPrime'
    where
        genPrime' g = do n' <- randInt g
                         prime <- isPrime k n'
                         if prime then pure n' else genPrime' g
        k = max (2556 `div` n) 2 -- calculate k for an error rate of 2^(-80)

isPrime :: Int -> Integer -> IO Bool
isPrime _ 2 = pure True
isPrime k n
    | divPrime n                = pure False   -- if divisible by small primes
    | powMod 210 (n - 1) n /= 1 = pure False   -- base-210 fermat test
    | otherwise                 = isPrime' k n -- miller-rabin primality test

primes :: [Integer]
primes = sieve [2..5000]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p > 0]

divPrime :: Integer -> Bool
divPrime n = (length . filter (== 0) . map (n `mod`)) primes > 0

isPrime' :: Int -> Integer -> IO Bool 
isPrime' k n = getStdGen >>= pure . and . primeList k n d r
    where
        (d, r) = primeForm n

primeForm :: Integer -> (Integer, Integer)
primeForm n = primeForm' (n - 1) 0
    where
        primeForm' d r | even d    = primeForm' (d `div` 2) (r + 1)
                       | otherwise = (d, r)

primeList :: Int -> Integer -> Integer -> Integer -> StdGen -> [Bool]
primeList k n d r g
    | k == 0    = []
    | otherwise = (checkPrime (powMod a d n) n r):primeList (k - 1) n d r g'
    where
        (a, g') = randomR (2, n - 2) g

checkPrime :: Integer -> Integer -> Integer -> Bool
checkPrime x n r
    | x == 1 || x == n - 1 = True
    | otherwise            = primeTest x n r

primeTest :: Integer -> Integer -> Integer -> Bool
primeTest x n r = let x' = powMod x 2 n
                  in primeTest' x' n r
    where
        primeTest' x' n' r' | x' == n' - 1       = True
                            | x' == 1 || r' == 1 = False
                            | otherwise          = primeTest x n (r - 1)

