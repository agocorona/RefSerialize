{-# LANGUAGE
             ScopedTypeVariables
             ,TypeSynonymInstances
             ,FlexibleInstances
             ,MultiParamTypeClasses
          #-}
{-
:l demos/pr.hs
getDeserial (runRSView (parseStr undefined) "1") :: Maybe Int
-}
import Control.Applicative
import Data.Monoid


data RS v a= RS v  (Maybe a)

newtype RSView v a=  RSView{runRSView :: (v -> (RS v a,v))}

instance Functor (RSView v) where
  fmap f (RSView p)= RSView $ \v  -> let (RS v a,r) =p v  in   (RS v ( fmap f a),r)

instance Monoid v => Applicative( RSView v) where
  pure a  = RSView ( \v  -> (RS  mempty $ Just a,v))
  RSView f <*> RSView g= RSView ( \v  ->
                   let (RS v1 k,r)  = f v 
                       (RS v2 x,r2) = g r
                   in  (RS (mappend v1 v2) (k <*> x),r2))


getSerial  (RS v _,_)= v
getDeserial (RS _ x,_)= x

class  SerialLet a v where
  parse ::   a -> RSView v a

  serial :: a -> v
  serial x= getSerial $ (runRSView $ parse x) undefined

  deserial :: v -> Maybe a
  deserial str= getDeserial ( (runRSView ( parse undefined)) str)



pString :: (Read a, Show a)=> a -> RSView String a
pString field = RSView $   \str ->
       case readsPrec 1 str of
          [] -> (RS (show field++" ") Nothing, str)
          ((x,r):_) -> (RS (show field++" ") (Just x), r)

str s= RSView ( \st -> (RS (s++" ") undefined  , drop (length s) st))

data P a = P a deriving (Read, Show)

instance  SerialLet (P  Int) String where
    parse (P x ) = P <$>  pString x



main =  do
--   putStr $ serial $ P "hi" (2::Int)
   print (deserial "P  1"  :: Maybe (P Int))


main2=
     print $ (  getDeserial (runRSView ( P <$> pString undefined) "1") :: Maybe (P Int))

