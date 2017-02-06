{-# LANGUAGE
             ScopedTypeVariables
             ,TypeSynonymInstances
             ,FlexibleInstances
             ,MultiParamTypeClasses
             ,FunctionalDependencies
          #-}
{-
:l demos\parseLets.Monad.hs
-}
import Control.Applicative
import Data.Monoid
import System.IO.Unsafe
import Control.Exception  as CE
import Data.List(isPrefixOf)
import Data.Maybe
import Debug.Trace
import qualified Data.ListLike as LL

(!>)= flip trace

data RS n m v a= RS (n v)  ( Maybe(m (Maybe a)))

newtype RSView n m v a=  RSView{runRSView :: RS n m v a}

instance Functor m => Functor (RSView n m v) where
  fmap f (RSView p)=RSView $        let RS v1 x= p
                                    in RS v1 $ fmap ( fmap(fmap f))  x


instance (Monoid v,Monad n,Monad m
          , Applicative m,Executable2 m v
          , Executable n) => Applicative( RSView n m v) where
  pure a  = RSView $ RS  (return mempty) $  Just $ return (Just a)
  RSView f <*> RSView g= RSView $ 
                   let RS mv1 mk  = f 
                       RS mv2 mx  = g 
                   in  RS ( do
                             v1 <- mv1
                             v2 <- mv2
                             return $ mappend v1 v2)
                        $ case mk of
                             Nothing -> Nothing
                             Just f  -> case mx of
                                 Nothing -> Nothing
                                 Just x  -> f <*> x



instance  (Monoid v,Monad n,Monad m
          , Applicative m,Executable n
          , Executable2 m v) => Alternative (RSView n m v) where
  empty= RSView $ RS (return mempty)  (return Nothing)
  RSView f <|> RSView g= RSView $ 
                   let rs@(RS v1 mk)  = f 
                   in
                       case mk of
                         Just _  -> rs
                         Nothing -> g 

class Monad m => Executable m where
  exec :: m a ->  a

class Monad m => Executable2 m a where
  exec2 :: m b -> a -> b

class  ParseLet n m v a | v ->n, a -> m, n ->m, m-> n where
    parse :: Maybe a -> RSView n m v a -- must not use pattern match

deserial :: ParseLet n m v a =>  v -> Maybe  a
deserial str=
 let RS d s= runRSView $ parse Nothing
 in exec2 d str


serial ::  ParseLet n m v a => a ->  v
serial x    = exec $ getSerial $ (runRSView $ parse (Just x))
   where
   getSerial  (RS v _)= v

sel f mpx= unsafePerformIO $
       CE.handle (\(e:: SomeException) -> return Nothing)
       $ let x= f $ fromJust mpx in x `seq` return (Just x)



--
--pString :: (Read a, Show a)=>  Maybe a -> RSView n m String a
--pString (Just fpx)= RSView $ \str ->  (RS (show$ fpx) (Just fpx),str)
--
--
--pString Nothing  = RSView $ \str ->
--          case readsPrec  1 str of
--                  []      ->  (RS " " Nothing, str)
--                  (x,r):_ ->  (RS " " (Just x), r)
--
----   `CE.catch` \(e :: SomeException) -> return (RS mempty Nothing, str))
--
----str :: String -> RSView String ()
str s= RSView ( \st ->
   let readit= if LL.isPrefixOf s st then Just (return()) else Nothing
   in (RS (return $ s++" ") (return readit) , drop (length s) st))


--
--data P = I Int | S String deriving (Read, Show)
--
--instance   ParseLet P String where
--    parse mpx  =   I <$> (str "I" *> pString (sel (\(I x) -> x) mpx ))
--              <|>  S <$> (str "S" *> pString (sel (\(S s) -> s) mpx ))
--
--main =  do
--   putStrLn . serial $ S "hi"
--   print (deserial "I 2" :: Maybe P )
--
--
