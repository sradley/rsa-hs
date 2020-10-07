module Rsa.Random
( CryptoGen (..)
, getCryptoGen
, randInt
) where

import           GHC.IO.Handle.Types (Handle)
import           System.IO           (openFile, IOMode (ReadMode))
import qualified Data.ByteString     as BS
import           Data.Bits

-- |...
data CryptoGen = CryptoGen Int Handle

-- | Initialises a new random number generator.
getCryptoGen :: Int -> IO CryptoGen
getCryptoGen n =
    openFile "/dev/urandom" ReadMode >>= pure . CryptoGen (n `div` 8)

-- |Generates a cryptographically secure random integer of size n-bits.
randInt :: CryptoGen -> IO Integer
randInt g = randBytes g >>= pure . (.|. 1) . bytesToInt . setFirstBit

randBytes :: CryptoGen -> IO [Integer]
randBytes (CryptoGen n h) = BS.hGet h n >>= pure . mapToInt 
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
