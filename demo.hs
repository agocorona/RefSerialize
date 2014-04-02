{-# OPTIONS -fglasgow-exts -XOverlappingInstances  -XUndecidableInstances -XOverloadedStrings    #-}
module Main where

import System.Mem.StableName
import System.IO.Unsafe
import Data.RefSerialize
import Data.Monoid
import Data.ByteString.Lazy.Char8(unpack)

-- we use a default instance for show and read instances
instance (Show a, Read a )=> Serialize a where
  showp= showpText
  readp= readpText

main=  do
   let x = (5 :: Int)
   let xss = [[x,x],[x,x]]
   let str= rShow xss
   putStrLn $ unpack str
   putStrLn "read the output back"
   let y = rRead  str :: [[Int]]
   print y
   print $ y==xss







