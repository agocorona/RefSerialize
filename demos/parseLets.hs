{-# LANGUAGE
             ScopedTypeVariables
             ,TypeSynonymInstances
             ,FlexibleInstances
             ,MultiParamTypeClasses

          #-}

import Control.Applicative
import Data.Monoid
import System.IO.Unsafe
import Control.Exception  as CE
import Data.List(isPrefixOf)
import Data.Maybe
import Debug.Trace
(!>)= flip trace

data RS v a= RS v  (Maybe a)

newtype RSView v a=  RSView{runRSView :: (v -> (RS v a,v))}

instance Functor (RSView v) where
  fmap f (RSView p)=RSView $  \v -> let (RS v1 x, r)= p v
                                    in (RS v1 (fmap f x),r)


--  RSView $ \v  -> let (RS v1 a,r) =p v
--                                     in  (RS v1 ( fmap f a),r)

instance Monoid v => Applicative( RSView v) where
  pure a  = RSView ( \v  -> (RS  mempty $ Just a,v))
  RSView f <*> RSView g= RSView ( \v  ->
                   let (RS v1 k,r)  = f v 
                       (RS v2 x,r2) = g r
                   in  (RS (mappend v1 v2) (k <*> x),r2))

instance  Monoid v => Alternative (RSView v) where
  empty= RSView $ \v -> (RS mempty Nothing, v)
  RSView f <|> RSView g= RSView ( \v  ->
                   let rs@(RS v1 k,r)  = f v 

                   in case k of
                     Just _  -> rs
                     Nothing -> g v )



class Monoid v =>  ParseLet a v where
  parse :: Maybe a -> RSView v a -- must not use pattern match

serial :: ParseLet a v => a -> v
serial x    = getSerial $ (runRSView $ parse  (Just x)) mempty
   where
   getSerial  (RS v _,_)= v

deserial :: ParseLet a v =>  v -> Maybe a
deserial str= getDeserial ( (runRSView ( parse Nothing)) str)
   where
   getDeserial (RS _ x,_)= x

sel f mpx= unsafePerformIO $
   CE.handle (\(e:: SomeException) -> return Nothing)
   $ let x= f $ fromJust mpx in x `seq` return (Just x)




pString :: (Read a, Show a)=>  Maybe a -> RSView String a
pString (Just fpx)= RSView $ \str ->  (RS (show$ fpx) (Just fpx),str)


pString Nothing  = RSView $ \str ->
          case readsPrec  1 str of
                  []      ->  (RS " " Nothing, str)
                  (x,r):_ ->  (RS " " (Just x), r)

--   `CE.catch` \(e :: SomeException) -> return (RS mempty Nothing, str))

--str :: String -> RSView String ()
str s= RSView ( \st ->
   let readit= if isPrefixOf s st then Just() else Nothing
   in (RS (s++" ") readit , drop (length s) st))



data P = I Int | S String deriving (Read, Show)

instance   ParseLet P String where
    parse mpx  =   I <$> (str "I" *> pString (sel (\(I x) -> x) mpx ))
              <|>  S <$> (str "S" *> pString (sel (\(S s) -> s) mpx ))

main =  do
   putStrLn . serial $ S "hi"
   print (deserial "I 2" :: Maybe P )


