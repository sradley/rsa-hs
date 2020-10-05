module Main where

import Rsa

main :: IO ()
main = do key <- generate 512 
          let enc = encrypt key 20
          print $ enc
          print $ decrypt key enc
          
