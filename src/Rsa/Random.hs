module Rsa.Random
( RandGen (..)
, getRandGen
, randInt
) where

import           GHC.IO.Handle.Types (Handle)
import           System.IO           (openFile, IOMode (ReadMode))
import qualified Data.ByteString     as BS
import           Data.Bits

-- |...
data RandGen = RandGen Int (IO Handle)

-- | Initialises a new random number generator.
getRandGen :: Int -> RandGen
getRandGen n = RandGen (n `div` 8) (openFile "/dev/urandom" ReadMode)

-- |Generates a cryptographically secure random integer of size n-bits.
randInt :: RandGen -> IO Integer
randInt g = randBytes g >>= pure . (.|. 1) . bytesToInt . setFirstBit

randBytes :: RandGen -> IO [Integer]
randBytes (RandGen n h) = h >>= (\x -> BS.hGet x n) >>= pure . mapToInt 
    where
        mapToInt xs = map fromIntegral $ BS.unpack xs

bytesToInt :: [Integer] -> Integer
bytesToInt xs = bytesToInt' (length xs) xs
    where
        bytesToInt' :: Int -> [Integer] -> Integer
        bytesToInt' _ []     = 0
        bytesToInt' i (y:ys) = (y ^ i) + (bytesToInt' (i-1) ys) 

setFirstBit :: [Integer] -> [Integer]
setFirstBit []     = []
setFirstBit (x:xs) = (x .|. 128):xs
