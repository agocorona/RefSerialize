{-# OPTIONS -XOverlappingInstances
            -XTypeSynonymInstances
            -XFlexibleInstances
            -XUndecidableInstances
            -XOverloadedStrings
              #-}

-----------------------------------------------------------------------------
--
-- Module      :  Data.RefSerialize
-- Copyright   :  Alberto Gómez Corona
-- License     :  see LICENSE
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental


{- | Read, Show and Data.Binary do not check for repeated references to the same address.
     As a result, the data is duplicated when serialized. This is a waste of space in the filesystem
     and  also a waste of serialization time. but the worst consequence is that, when the serialized data is read,
     it allocates multiple copies for the same object when referenced multiple times. Because multiple referenced
     data is very typical in a pure language such is Haskell, this means that the resulting data loose the beautiful
     economy of space and processing time that referential transparency permits.

     This package leverages Show, Read and Data.Binary instances while it permits textual as well as binary serialization
      keeping internal  references.

     Here comes a brief tutorial:

     @runW applies showp, the serialization parser of the instance Int for the RefSerialize class

    Data.RefSerialize>let x= 5 :: Int
    Data.RefSerialize>runW $ showp x
    "5"

    every instance of Read and Show is an instance of RefSerialize. for how to construct showp and readp parsers, see the demo.hs

    rshowp is derived from showp, it labels the serialized data with a variable name

    Data.RefSerialize>runW $ rshowp x
    " v8 where {v8= 5; }"

    Data.RefSerialize>runW $ rshowp [2::Int,3::Int]
    " v6 where {v6= [ v9,  v10]; v9= 2; v10= 3; }"

    while showp does a normal show serialization

    Data.RefSerialize>runW $ showp [x,x]
    "[5, 5]"

    rshowp variables are serialized memory references: no piece of data that point to the same address is serialized but one time

    Data.RefSerialize>runW $ rshowp [x,x]
    " v9 where {v6= 5; v9= [ v6, v6]; }"



    "this happens recursively"

    Data.RefSerialize>let xs= [x,x] in str = runW $ rshowp [xs,xs]
    Data.RefSerialize>str
    " v8 where {v8= [ v10, v10]; v9= 5; v10= [ v9, v9]; }"

    the rshowp serialized data is read with rreadp. The showp serialized data is read by readp

    Data.RefSerialize>let xss= runR rreadp str :: [[Int]]
    Data.RefSerialize>print xss
    [[5,5],[5,5]]

    this is the deserialized data

    the deserialized data keep the references!! pointers are restored! That is the whole point!

    Data.RefSerialize>varName xss !! 0 == varName xss !! 1
    True


    rShow= runW rshowp
    rRead= runR rreadp

    Data.RefSerialize>rShow x
    " v11 where {v11= 5; }"


    In the definition of a referencing parser non referencing parsers can be used and vice-versa. Use a referencing parser
    when the piece of data is being referenced many times inside the serialized data.

    by default the referencing parser is constructed by:


    rshowp= insertVar showp
    rreadp= readVar readp
    but this can be redefined. See for example the instance of [] in RefSerialize.hs

    This is an example of a showp parser for a simple data structure.

    data S= S Int Int deriving ( Show, Eq)

    instance  Serialize S  where
        showp (S x y)= do
--                      insertString "S"
                        rshowp x  -- rshowp parsers can be inside showp parser
                        rshowp y


       readp =  do
                        symbol "S"     -- I included a (almost) complete Parsec for deserialization
                        x <- rreadp
                        y <- rreadp
                        return $ S x y

    there is a mix between referencing and no referencing parser here:

    Data.RefSerialize>putStrLn $ runW $ showp $ S x x
    S  v23 v23 where {v23= 5; }@

-}





module Data.RefSerialize
(
     module Data.RefSerialize.Parser
    ,Serialize(
        showp
       ,readp
     )
    ,Context
    ,newContext
    ,rshowp
    ,rreadp
    ,showps
    ,showpText
    ,readpText
    ,takep
    ,showpBinary
    ,readpBinary
    ,insertString
    ,insertChar
    ,rShow
    ,rRead
    ,insertVar
    ,readVar
    ,varName
    ,runR
    ,runRC
    ,runW

    ,readHexp
    ,showHexp
    ,getContext

)

 where

import Data.RefSerialize.Serialize
import Data.RefSerialize.Parser
import Unsafe.Coerce
import Data.Char(isAlpha, isSpace, isAlphaNum)
import Numeric(readHex,showHex)
import Data.ByteString.Lazy.Char8 as B
--import Data.ByteString(breakSubstring)
import Debug.Trace
import Data.Binary
import System.IO.Unsafe
import qualified Data.Map as M


newContext :: IO Context
newContext  = Data.RefSerialize.Serialize.empty

class Serialize c where
   showp :: c -> ST ()     -- ^ shows the content of a expression, must be  defined by the user
   readp ::  ST c          -- ^ read the content of a expression, must be user defined

-- | insert a reference (a variable in the where section).

-- @rshowp  = insertVar  showp @
rshowp :: Serialize c => c -> ST ()
rshowp  = insertVar  showp

 --  | read a variable in the where section (to use for deserializing rshowp output).

 --   @rreadp  = readVar  readp@
rreadp ::  Serialize c => ST c
rreadp = readVar  readp

{-
#ifdef Axioms

   serializeAxioms: Axioms c
   serializeAxioms= axioms{
         unary=   [Axiom "reverse"
                            (\x ->  let str= rShow x
                                        y = rRead xtr
                                    in  y== x)

                   AxioM "pointer equality"
                           (\x ->   let str= rShow[x,x]
                                        [y,z] = rRead str
                                    in varName y== varName z)
                  ]
        }
#endif
-}



-- | return the serialized list of variable values
-- useful for delayed deserialzation of expressions, in case of dynamic variables were deserialization
-- is done when needed, once the type is known with `runRC`

getContext :: ST (Context, ByteString)
getContext = ST(\(Stat(c,s,v)) -> Right (Stat (c,s,v), (c,v)))

-- | use the rshowp parser to serialize the object
-- @ rShow c= runW  $  rshowp c@
rShow :: Serialize c => c -> ByteString
rShow c= runW  $  showp c

-- | deserialize  through the rreadp parser
-- @ rRead str= runR rreadp $ str@
rRead :: Serialize c => ByteString -> c
rRead str= runR readp $ str

readHexp :: (Num a, Integral a) => ST a
readHexp = ST(\(Stat(c,s,v)) ->
   let us= unpack s
       l=  readHex  us
   in if Prelude.null l then Left . Error $  "readHexp: not readable: " ++ us
         else let ((x,str2):_)= l
              in Right(Stat(c, pack $ Prelude.dropWhile isSpace str2,v),x) )
   <?> "readHexp "



showHexp :: (Num a,Integral a,Show a) => a -> ST ()
showHexp var= ST(\(Stat(c,s,v)) ->  Right(Stat(c, s `append` " " `append` (pack $ showHex var ""),v),()))  <?> "showHexp "

-- |if a is an instance of Show, showpText can be used as the showp method
-- the drawback is that the data inside is not inspected for common references
-- so it is recommended to create your own readp method for your complex data structures
showpText :: Show a => a -> ST ()
showpText var= ST(\(Stat(c,s,v)) ->  Right(Stat(c, s `append` (snoc (pack $ show var) ' ') ,v),()))   <?> "showp: show "

-- |if a is an instance of Read, readpText can be used as the readp method
-- the drawback is that the data inside is not inspected for common references
-- so it is recommended to create your own readp method for your complex data structures
readpText :: Read a => ST a
readpText = ST(\(Stat(c,s,v)) ->
   let us= unpack s
       l=  readsPrec 1 us
   in if Prelude.null l then Left . Error $  "not readable: " ++ us
         else let ((x,str2):_)= l
              in Right(Stat(c, pack $ Prelude.dropWhile isSpace str2,v),x) )
   <?> "readp: readsPrec "



-- |  deserialize the string with the parser
runR:: ST a -> ByteString ->  a
runR p str=unsafePerformIO $ do
    c <- newContext
    let (struct, vars)= readContext whereSep str
    return $ runRC (c, vars) p struct

-- | read an expression with the variables definedd in a context passed as parameter.
runRC :: (Context, ByteString) -> ST a -> ByteString ->  a
runRC (c,vars) (ST f) struct=
  case   f (Stat(c,struct,vars) ) of
      Right (Stat _, a) -> a
      Left (Error s) -> error s

whereSep= "\r\nwhere{\r\n "

-- |   serialize x with the parser
runW :: ST () -> ByteString
runW (ST f) = unsafePerformIO $ do
      c <- newContext
      return $ case f (Stat(c,"",""))  of
              Right (Stat (c,str,_), _) ->
                let scontext= assocs c
                    vars= B.concat $ Prelude.map (\(n,(_,_,v))->"v" `append`  (pack $ show n)  `append`  "= "  `append`  v  `append`  ";\r\n ")  scontext
                    strContext= if Prelude.null scontext  then "" else  whereSep `append` vars  `append`  "\r\n}"
                in  str  `append`  strContext

              Left (Error s) -> error s



-- | output the string of the serialized variable
showps :: Serialize a =>  a -> ST ByteString
showps x= ST(\(Stat(c,s,v))->
 let
    ST f= showp x
    Right (Stat (c',str,_), _) = f  (Stat(c,"",v))

 in Right(Stat(c',s ,v), str))



-- | insert a variable at this position. The expression value is inserted in the "where" section if it is not already
-- created. If the address of this object being parsed correspond with an address already parsed and
-- it is in the where section, then the same variable name is used
--   @runW showp (1::Int)                                -> "1"
--   runW (insertVar showp) (1::Int)                ->  v1 where { v1=1}
--   runW (insertVar showp) [(1::Int) ,1]        -> [v1.v1] where { v1=1}@
--   This is useful when the object is referenced many times

insertVar :: (a -> ST ()) -> a -> ST ()
insertVar parser x= ST(\(Stat(c,s,v))->
 let mf = trytofindEntireObject x c in
 case mf of
   Just  var ->  Right(Stat(c,s `append` " " `append` var,v),())
   Nothing ->
         let
            ST f= parser x
            Right (Stat (c',str,_), _) = f  (Stat(c,"",v))

         in Right(Stat(addc str c',s `append` (cons ' ' varname) ,v), ()))
 where
  addc str c= insert ( hash) (st,unsafeCoerce x,  str) c
  (hash,st) = hasht x
  varname=  pack$ "v" ++ show hash

  trytofindEntireObject x c=
         case Data.RefSerialize.Serialize.lookup  hash  c  of
           Nothing -> Nothing
           Just _  -> Just varname

-- | inform if the expression iwas already referenced and return @Right varname@
--  otherwise, add the expression to the context and giive it a name and return  @Left varname@
-- The varname is not added to the serialized expression. The user must serialize it
-- This is usefu for expressions that admit different syntax depending or recursiviity, such are lists

isInVars :: (a -> ST ()) -> a -> ST (Either ByteString ByteString)
isInVars parser x= ST(\(Stat(c,s,v))->
 let mf = trytofindEntireObject x c in
 case mf of
   Just  var ->  Right(Stat(c,s,v),Right var)
   Nothing ->
         let
            ST f= parser x
            Right (Stat (c',str,_), _) = f  (Stat(c,"",v))

         in Right(Stat(addc str c',s ,v), Left varname))
 where
  addc str c= insert ( hash) (st,unsafeCoerce x,  str) c
  (hash,st) = hasht x
  varname=  pack$ "v" ++ show hash

  trytofindEntireObject x c=
         case Data.RefSerialize.Serialize.lookup  hash  c  of
           Nothing -> Nothing
           Just _  -> Just varname



-- | deserialize a variable serialized with insertVar. Memory references are restored
readVar :: Serialize c => ST c -> ST c
readVar (ST f)=  ST(\(Stat(c,s,v))->
     let
       s1= B.dropWhile isSpace s
       (var, str2) = B.span isAlphaNum s1
       str3= B.dropWhile isSpace str2
       nvar= numVar $ unpack var

     in  if B.null var then Left (Error "expected variable name" )
         else
          case  trytofindEntireObject nvar c of

           Just  (_,x,_) ->  Right(Stat(c,str3,v),unsafeCoerce x)
           Nothing ->
            let
               (_, rest)= readContext (var `append` "= ") v

            in if B.null rest then Left (Error ( "RedSerialize: readVar: " ++ unpack var ++ "value not found" ))
               else  case f  (Stat(c,rest,v)) of

                 Right (Stat(c',s',v'),x) ->
                   let c''= insert nvar ( undefined, unsafeCoerce x,  "") c'
                   in  Right (Stat(c'', str3,v),x)

                 err -> err)
  where
  trytofindEntireObject x c=
         case Data.RefSerialize.Serialize.lookup   x  c  of
           Nothing -> Nothing
           justx   -> justx


-- |  Write a String in the serialized output with an added whitespace. Deserializable with `symbol`
insertString :: ByteString -> ST ()
insertString s1= ST(\(Stat(c,s,v)) ->  Right(Stat(c, s  `append` ( snoc s1 ' '),v),()))

-- | Write a char in the serialized output (no spaces)
insertChar :: Char -> ST()
insertChar car= ST(\(Stat(c,s,v)) ->  Right(Stat(c, snoc s car,v),()))
--

-- -------------Instances

instance Serialize String where
    showp = showpText
    readp = readpText


instance  Serialize a => Serialize [a] where
   showp []= insertString "[]"
   showp (x:xs)= do
           insertChar '['
           rshowp x
           mapM f xs
           insertString "]"
           where
           f :: Serialize a => a -> ST ()
           f x= do
              insertChar ','
              rshowp x


   readp = (brackets . commaSep $ rreadp)   <?> "readp:: [] "

{-
instance Serialize a => Serialize [a] where
 showp xs= showpl [] xs
   where
   showpl res []= bracketdisp $ Prelude.reverse res
   showpl res xs= do
        is <- isInVars showp xs
        case is of
            Right v ->parensdisp  (Prelude.reverse res) v
            Left  v -> showpl (v:res) xs

   parensdisp xs t= do
           insertChar '('
           disp ':' xs
           insertChar ':'
           insertString t
           insertString ")"

   bracketdisp []= insertString "[]"
   bracketdisp xs= do
           insertChar '['
           disp ',' xs
           insertString "]"

   disp sep (x:xs)= do
           insertString x
           mapM f xs
           where

           f x= do
              insertChar sep
              insertString x

 readp= choice [bracketsscan, parensscan]  <?> "readp:: [] "
   where
   bracketsscan= (brackets . commaSep $ rreadp)   <?> "readp:: [] "
   parensscan=parens $ do
       xs <- many(rreadp >>= \x -> symbol ":" >> return x)
       end <- rreadp
       return $ xs ++ end


-}



instance (Serialize a, Serialize b) => Serialize (a, b) where
    showp (x, y)= do
            insertString  "("
            rshowp x
            insertString ","
            rshowp y
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            return (x,y))
            <?> "rreadp:: (,) "

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b,c) where
    showp (x, y, z)= do
            insertString "("
            rshowp x
            insertString ","
            rshowp y
            insertString ","
            rshowp z
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            comma
            z <- rreadp
            return (x,y,z))
            <?> "rreadp:: (,,) "

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b,c, d) where
    showp (x, y, z, t)= do
            insertString "("
            rshowp x
            insertString ","
            rshowp y
            insertString ","
            rshowp z
            insertString ","
            rshowp t
            insertString ")"


    readp =  parens (do
            x <- rreadp
            comma
            y <- rreadp
            comma
            z <- rreadp
            comma
            t <- rreadp
            return (x,y,z,t))
            <?> "rreadp:: (,,,) "

instance (Serialize a, Ord a, Serialize b) => Serialize (M.Map a b) where
    showp m= showp $ M.toList m
    readp= do
           list <- readp  -- :: ST [(a,b)]
           return $ M.fromList list




instance Serialize a => Serialize (Maybe a) where
    showp Nothing = insertString "Nothing"
    showp (Just x) =do
          insertString "Just"
          showp x

    readp =  choice [rNothing, rJust] where
      rNothing = symbol "Nothing" >> return Nothing
      rJust =  do
         symbol "Just"
         x <- readp
         return $ Just x

instance (Serialize a, Serialize b) => Serialize (Either a b) where
    showp (Left x) = do
       insertString "Left"
       rshowp x

    showp (Right x) = do
       insertString "Right"
       rshowp x

    readp =  choice [rLeft, rRight] where
      rLeft = symbol "Left" >> rreadp >>= \x -> return $ Left x
      rRight = symbol "Right" >> rreadp >>= \x -> return $ Right x




-- binary serialization


binPrefix=   "Bin "
binPrefixSp= append (pack binPrefix) " "

-- | serialize a variable which has a Binary instance
showpBinary :: Binary a => a -> ST ()
showpBinary x = do
    let s = encode x
    let n = pack . show $ B.length s
    insertString $  binPrefixSp `append` n `append` " " `append` s

-- | deserialize a variable serialized by `showpBinary`
readpBinary :: Binary a => ST a
readpBinary = do
      symbol binPrefix
      n     <- integer
      str   <- takep $ fromIntegral n
      let x = decode str
      return x

-- return n chars form the serialized data
takep :: Int -> ST ByteString
takep n= take1 "" n
  where
  take1 s 0= return s
  take1 s n= anyChar >>= \x -> take1 (snoc s x) (n-1)

-- | default instances

instance (Show a, Read a )=> Serialize a where
  showp= showpText
  readp= readpText



