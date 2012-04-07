{-# OPTIONS -fglasgow-exts -XOverlappingInstances  -XUndecidableInstances    #-}
module Main where

import System.Mem.StableName
import System.IO.Unsafe
import Data.RefSerialize

--simple data structure
data S= S Int Int deriving ( Show, Eq)


instance  Serialize S  where
    showp (S x y)= do
                    xs <- rshowp x  -- rshowp parsers can be inside showp parser
                    ys <- rshowp y
                    return $ "S "++xs++" "++ys


    readp =  do
                    symbol "S"
                    x <- rreadp
                    y <- rreadp
                    return $ S x y

----------------- a more complex structure with mixed record and array with default read/show type serialization ----

data Data = Data Int String deriving (Read,Show)

data Stat a= Workflows [String]
           | Stat{ wfName :: String, state:: Int, index :: Int, recover:: Bool, sync :: Bool , resource :: [a]}
           | I a
           deriving (Read,Show)

-- the parser definitions for this structure

instance Serialize a =>  Serialize (Stat a) where
    showp (Workflows list)= do
          str <- showp list
          return $ "StatWorkflows "++ str

    showp (I x) = return $ "I " ++ rShow x

    showp  (Stat wfName state index recover sync resource)= do
       parsea <- rshowp resource     --creates a variable
       return $ "Stat "++ show wfName ++" "++ show state++" "++show index++" "++show recover++" "++ show sync ++ parsea

    readp = choice [rStat, rData, rWorkflows] where --choice is a exported parser (Parsec.Token interface is included)
        rStat= do
              symbol "Stat"
              wfName <- stringLiteral    -- various parsec parsers are used
              state <- integer
              index <- integer
              recover <- bool
              sync <- bool
              resource <- rreadp   -- read the variable
              return $ Stat wfName (fromIntegral state) (fromIntegral index) recover sync resource

        rData= do
               symbol "I"
               a <- readp
               return $ I a

        rWorkflows= do
               symbol "StatWorkflows"
               list <- readp
               return $ Workflows list


main=  do
   let x = (5 :: Int)
   putStrLn $ runW $ showp $ S x x
   let xss = [[x,x],[x,x]]
   let str= rShow xss
   putStrLn str
   let y = rRead  " v10 where {v6= [ v8, v8]; v8= 5; v9= [v8, v8]; v10= [ v6,  v9]; }" ::[[Int]]
   print y
   putStrLn "instance (Show a, Read a) => Serialize a "
   putStr "rShow 10="
   putStrLn $ rShow (10 :: Int)

   putStrLn "serialize String's"
   let x= "hello"
   let str= rShow x
   putStrLn $ "rShow "++ str++"= "++str

   let y= rRead str :: String
   print y
   print $ x==y

   putStrLn "serialize [a] "
   let xs= take 2 $ repeat (Data 0 "")

   let xss= take 2 $ repeat xs
   let str= rShow xss
   putStrLn str
   let x= rRead str :: [[Data]]



   let x= 1 :: Int
   let xs= take 5 $ repeat x

   print xs
   putStr "rShow xs="
   let str= rShow  xs
   putStrLn str


   putStrLn "using the RefSerialize instance of Stat (see definition in this file)"
   let stat0 = Stat{ wfName="", state=0, index=0, recover=False, resource=[], sync= True}

   let data0= Data 0 ""

   let str = rShow  stat0{resource= (take 2 $ repeat  data0) ++ (take 2 $ repeat (Data 1 "1")) }
   putStrLn "references to the same address are identified by rshowp. they point to the same variable in the serialized data"
   putStrLn $ "rShow "++ show data0 ++"= "++ str
   let stat1= rRead str :: Stat Data

   putStrLn "data that point to the same variable when serializeds point to the same memory address when deserialized"

   let addr x= (hashStableName . unsafePerformIO . makeStableName) x
   let x= (resource stat1 !! 0)
   putStr "first element of the resource list= "
   print x
   putStr "address of this element= "
   print $ addr x
   let y= (resource stat1 !! 1)
   putStr "second element of the resource list= "
   print y
   putStr "address of this element= "
   print $ addr y
   print $ addr y== addr x



