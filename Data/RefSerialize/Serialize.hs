{-# OPTIONS -XOverlappingInstances
            -XTypeSynonymInstances
            -XFlexibleInstances
            -XUndecidableInstances
            -XOverloadedStrings
            -XNoMonomorphismRestriction
              #-}
module Data.RefSerialize.Serialize where
import GHC.Exts
import Unsafe.Coerce
import Data.List(isPrefixOf,insertBy,elem)
import Data.Char(isAlpha,isAlphaNum,isSpace,isUpper)

import System.Mem.StableName
import System.IO.Unsafe
import Control.Monad (MonadPlus(..))
import Data.ByteString.Lazy.Char8 as B
import qualified Data.HashTable  as HT
import Data.List(sortBy)
import Data.Ord


type MFun=  Char -- usafeCoherced to char to store simply the address of the function
type VarName = String
type ShowF= ByteString
type Context =  HT.HashTable Int (  StableName MFun, MFun,ShowF)

data Error= Error String
data Stat= Stat (Context, ByteString, ByteString)


-- HT to map
empty  =   HT.new (==) HT.hashInt

assocs = sortBy (comparing fst) . unsafePerformIO . HT.toList

insert  k v ht= unsafePerformIO $ HT.insert ht k v >> return ht

lookup  k ht= unsafePerformIO $ HT.lookup ht k

toList  = unsafePerformIO . HT.toList

fromList = unsafePerformIO . HT.fromList HT.hashInt


readContext :: ByteString -> ByteString -> (ByteString, ByteString)
readContext pattern str= readContext1  (pack "") str where

 readContext1 :: ByteString -> ByteString -> (ByteString, ByteString)
 readContext1 s str| B.null str = (s, pack "")
                   | pattern `B.isPrefixOf` str = (s, B.drop n str)
                   | otherwise=   readContext1 (snoc s (B.head str)) (B.tail str)
                    where n= fromIntegral $ B.length pattern


hasht x= unsafePerformIO $ do
       st <- makeStableName $! x
       return (hashStableName st,unsafeCoerce st)

-- !  two variables that point to the same address will have identical varname (derived from import System.Mem.StableName)varName:: a -> String
varName x= "v"++ (show . hash) x
  where hash x= let (ht,_)= hasht x in ht




numVar :: String -> Int
numVar "" = error "refSerialize: numVar: null variable"
numVar var= read  $ Prelude.tail var



