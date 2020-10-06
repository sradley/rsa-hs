module Rsa.Util
( powMod
, modInv
, egcd
) where

-- |...
powMod :: Integer -> Integer -> Integer -> Integer
powMod x 1 m = x `mod` m
powMod x p m 
    | even p    = let y = powMod x (p `div` 2) m
                  in (y * y) `mod` m
    | otherwise = let y = powMod x ((p - 1) `div` 2) m
                  in (x * y * y) `mod` m 

-- |...
modInv :: Integer -> Integer -> Maybe Integer
modInv a m = let modInv' (g, x, _) | g /= 1    = Nothing
                                   | otherwise = Just (x `mod` m)
             in modInv' $ egcd a m

-- |...
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let out (g, y, x) = (g, x - (b `div` a) * y, y)
           in out $ egcd (b `mod` a) a
