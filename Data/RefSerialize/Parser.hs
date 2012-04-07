{- |  A Parsec parser for the refSerialize monad. See package Parsec. all the functions have the same meaning
-}
module Data.RefSerialize.Parser( ST(..),(<?>),(<|>),char,anyChar, string, upper, space, digit
                 , sepBy, between, choice, option, notFollowedBy, many, manyTill, oneOf, noneOf
                 , bool

                 , charLiteral      -- :: ST Char
                 , stringLiteral    -- :: ST String
                 , natural          -- :: ST Integer
                 , integer          -- :: ST Integer
                 , float            -- :: ST Double
                 , naturalOrFloat   -- :: ST (Either Integer Double)
                 , decimal          -- :: ST Integer
                 , hexadecimal      -- :: ST Integer
                 , octal            -- :: ST Integer

                 , symbol           -- :: String -> ST String
                 , lexeme           -- :: forall a. ST a -> ST a
                 , whiteSpace       -- :: ST ()

                 , parens           -- :: forall a. ST a -> ST a
                 , braces           -- :: forall a. ST a -> ST a
                 , angles           -- :: forall a. ST a -> ST a
                 , brackets         -- :: forall a. ST a -> ST a
                 -- "squares" is deprecated

                 , semi             -- :: ST String
                 , comma            -- :: ST String
                 , colon            -- :: ST String
                 , dot              -- :: ST String
                 , semiSep          -- :: forall a . ST a -> ST [a]
                 , semiSep1         -- :: forall a . ST a -> ST [a]
                 , commaSep         -- :: forall a . ST a -> ST [a]
                 , commaSep1        -- :: forall a . ST a -> ST [a]


                 ) where
import Prelude hiding(head,tail, null)
import Control.Monad
import Data.Char(isUpper,isSpace,digitToInt)
import qualified Data.Map as M
import Data.RefSerialize.Serialize
import Data.ByteString.Lazy.Char8


data ST a= ST(Stat-> Either Error (Stat , a) )

-- | monadic serialization & deserialization
instance  Monad ST where
    return  x = ST (\s -> Right (s, x))
    ST g >>= f = ST (\s ->

                       case g s of
                        Right (s', x)->
                          let
                              ST fun  = f x
                          in  case  fun s' of
                               left@(Left msg) -> left
                               rigth->  rigth

                        Left msg -> Left msg

                    )

instance MonadPlus ST where
  mzero= ST (\(Stat (a,b,c)) -> Left $ Error "an error occurred")
  mplus p1 p2   = parsecPlus p1 p2

infixr 1 <|>
(<|>) = parsecPlus
infix  0 <?>

p <?> msg = label p msg

parsecPlus :: ST a -> ST a -> ST a
parsecPlus (ST p1) (ST p2)
    = ST (\state ->
        case (p1 state) of
          Left (Error s) -> case (p2 state) of
                                 Left (Error s') -> Left $ Error ( s++ "\n"++ s')
                                 consumed-> consumed
          other             -> other
      )


label :: ST a -> String -> ST a
label p msg
  = labels p [msg]

labels :: ST a -> [String] -> ST a
labels (ST p) msgs
    = ST (\state ->
        case (p state) of
          Left(Error reply) -> Left $  Error ( reply ++Prelude.concatMap ("\n in "++) msgs)

          other       -> other
      )

char :: Char -> ST Char

unexpectedEndOfInput= "unexpected end of input"
char c= ST(\(Stat(cs,s,v)) ->
   if null s then Left (Error $ unexpectedEndOfInput)
   else if c== head s then Right(Stat(cs,tail s,v), c)
   else Left (Error ( "char "++ c:" not match " ++ '\"':unpack s++"\"" )))


anyChar = ST(\(Stat(cs,s,v)) ->
    if null s then Left (Error $ unexpectedEndOfInput)
    else Right(Stat(cs,tail s,v), head s))

satisfy bf= ST(\(Stat(cs,s,v)) ->  let  heads= head s in
     if null s then Left (Error $ unexpectedEndOfInput)
     else if bf heads then  Right(Stat(cs,tail s,v), heads)
     else Left (Error ( "satisfy  not matching condition in " ++ '\"':unpack s++"\"" )))


upper = ST(\(Stat(cs,s,v)) ->  let  heads= head s in
     if null s then Left (Error $ unexpectedEndOfInput)
     else if isUpper (head s) then  Right(Stat(cs,tail s,v), head s)
     else Left (Error ( "upper  not matching condition in " ++ '\"':unpack s++"\"" )))


space =ST(\(Stat(cs,s,v)) ->  let  heads= head s in
     if null s then Left (Error $ unexpectedEndOfInput)
     else if isSpace heads then Right(Stat(cs,tail s,v), heads)
     else Left (Error ( "expected space at the head of " ++ unpack s )))


digit1 l1 l2= ST(\(Stat(cs,s,v)) -> let c= head s in  if c >= l1 && c <= l2  then Right(Stat(cs,tail s,v), c)
                                     else Left (Error ( "expected digit at the head of " ++ unpack s )))

empty = ST(\(Stat(cs,s,v)) ->   if null s  then Right(Stat(cs, s,v), ())
                                     else Left (Error ( "expected empty list" )))

octDigit= digit1 '0' '7'

digit= digit1 '0' '9'

hexDigit= ST(\(Stat(cs,s,v)) ->  let c= head s in if c >= '0' && c <= '9'  || c >= 'a' && c<='f'  || c >= 'A' && c <= 'F'  then Right(Stat(cs,tail s,v), c)
                                     else Left (Error ( "expected space at the head of " ++ unpack s )))

oneOf xs= ST(\(Stat(cs,s,v)) -> let c= head s in if c `Prelude.elem` xs then Right(Stat(cs,tail s,v), c)
                                     else Left (Error ( "expected digit at the head of " ++ unpack s )))

noneOf xs= ST(\(Stat(cs,s,v)) -> let c= head s in if not $ c `Prelude.elem` xs then Right(Stat(cs,tail s,v), c)
                                     else Left (Error ( "expected digit at the head of " ++ unpack s )))

try p= p

unexpected msg
    = ST (\state -> Left (Error $ msg++ "unexpected"))

sepBy1,sepBy :: ST a -> ST  sep -> ST  [a]
sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }
                        <?> "sepBy "
between open close p
                    = do{ open; x <- p; close; return x }

choice ps           = Prelude.foldr (<|>) mzero ps <?> "choice "

option x p          = p <|> return x


notFollowedBy p     = try (do{ c <- p; unexpected (show [c]) }
                           <|> return ()
                          )

                          <?> "notFollowedBy "

skipMany1 p         = do{ p; skipMany p }

skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()


manyTill p end      = scan
                    where
                      scan  = do{ end; return [] }
                            <|>
                              do{ x <- p; xs <- scan; return (x:xs) }


string ""=  return ""
string ys@(x:xs)= do
                  char x
                  string xs
                  return ys
                  <?> "string "++ys


bool = lexeme ( do{ symbol "True" ; return True} <|> do{ symbol "False" ; return False})   <?> "Bool"

many :: ST a -> ST [a]
many p = many1 p <|> return []
many1 :: ST a -> ST [a]
many1 p = do {a <- p; as <- many p; return (a:as)}


--from Token.hs
-----------------------------------------------------------
-- Bracketing
-----------------------------------------------------------
parens p        = between (symbol "(") (symbol ")") p <?> "parens "
braces p        = between (symbol "{") (symbol "}") p <?> "braces "
angles p        = between (symbol "<") (symbol ">") p <?> "angles "
brackets p      = between (symbol "[") (symbol "]") p <?> "brackets "

semi            = symbol ";"
comma           = symbol ","
dot             = symbol "."
colon           = symbol ":"

commaSep p      = sepBy p comma
semiSep p       = sepBy p semi

commaSep1 p     = sepBy1 p comma
semiSep1 p      = sepBy1 p semi


-----------------------------------------------------------
-- Chars & Strings
-----------------------------------------------------------
-- charLiteral :: ST Char
charLiteral     = lexeme (between (char '\'')
                                (char '\'' <?> "end of character")
                                characterChar )
                <?> "character"

characterChar   = charLetter <|> charEscape
                <?> "literal character"

charEscape      = do{ char '\\'; escapeCode }
charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



-- stringLiteral :: ST String
stringLiteral   = lexeme (
                do{ str <- between (char '"')
                                        (char '"' <?> "end of string")
                                        (many stringChar)
                ; return (Prelude.foldr (maybe id (:)) "" str)
                }
                <?> "literal string")

-- stringChar :: ST (Maybe Char)
stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape    = do{ char '\\'
                ;     do{ escapeGap  ; return Nothing }
                        <|> do{ escapeEmpty; return Nothing }
                        <|> do{ esc <- escapeCode; return (Just esc) }
                }

escapeEmpty     = char '&'
escapeGap       = do{ many1 space
                ; char '\\' <?> "end of string gap"
                }



-- escape codes
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

-- charControl :: ST Char
charControl     = do{ char '^'
                ; code <- upper
                ; return (toEnum (fromEnum code - fromEnum 'A'))
                }

-- charNum :: ST Char
charNum         = do{ code <- decimal
                                <|> do{ char 'o'; number 8 octDigit }
                                <|> do{ char 'x'; number 16 hexDigit }
                ; return (toEnum (fromInteger code))
                }

charEsc         = choice (Prelude.map parseEsc escMap)
                where
                parseEsc (c,code)     = do{ char c; return code }

charAscii       = choice (Prelude.map parseAscii asciiMap)
                where
                parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          = Prelude.zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        = Prelude.zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- naturalOrFloat :: ST (Either Integer Double)
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"


-- floats
floating        = do{ n <- decimal
                ; fractExponent n
                }


natFloat        = do{ char '0'
                ; zeroNumFloat
                }
                <|> decimalFloat

zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                        ; return (Left n)
                        }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat    = do{ n <- decimal
                ; option (Left n)
                                (fractFloat n)
                }

fractFloat n    = do{ f <- fractExponent n
                ; return (Right f)
                }

fractExponent n = do{ fract <- fraction
                ; expo  <- option 1.0 exponent'
                ; return ((fromInteger n + fract)*expo)
                }
                <|>
                do{ expo <- exponent'
                ; return ((fromInteger n)*expo)
                }

fraction        = do{ char '.'
                ; digits <- many1 digit <?> "fraction"
                ; return (Prelude.foldr op 0.0 digits)
                }
                <?> "fraction"
                where
                op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent'       = do{ oneOf  "eE"
                ; f <- sign
                ; e <- decimal <?> "exponent"
                ; return (power (f e))
                }
                <?> "exponent"
                where
                power e  | e < 0      = 1.0/power(-e)
                        | otherwise  = fromInteger (10^e)


-- integers and naturals
int             = do{ f <- lexeme sign
                ; n <- nat
                ; return (f n)
                }

-- sign            :: ST (Integer -> Integer)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat             = zeroNumber <|> decimal

zeroNumber      = do{ char '0'
                ; hexadecimal <|> octal <|> decimal <|> return 0
                }
                <?> ""

decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }


    -- number :: Integer -> ST Char -> ST Integer
number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = Prelude.foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }


-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------
symbol name
        = lexeme (string name)  <?> "symbol"

lexeme p
        = do{ x <- p; whiteSpace ; return x  }


--whiteSpace
whiteSpace  = skipMany (simpleSpace <?> "")


simpleSpace = skipMany1 (satisfy isSpace)


