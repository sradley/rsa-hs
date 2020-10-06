module Rsa.Util
( powMod
, modInv
, egcd
) where

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

powMod :: Integer -> Integer -> Integer -> Integer
powMod b e m = powMod' b e m 1

powMod' :: Integer -> Integer -> Integer -> Integer -> Integer
powMod' _ 0 _ r = r
powMod' b e m r
    | e `mod` 2 == 1 = powMod' (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powMod' b e m r = powMod' (b * b `mod` m) (e `div` 2) m r
