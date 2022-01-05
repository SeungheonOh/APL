{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Main

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import Control.Applicative hiding (many, some)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import Data.Data
import Language.Haskell.TH (lamE)

type Parser = Parsec Void T.Text

newtype APLAxis = Axis [Int] deriving (Show, Data)

data APLOperator = Op String
                 | AntiOp String
                 deriving (Show, Data)

data APLElem = Alpha
             | Omega
             | Number Int
             | AntiElem String
             | MonadicOp APLOperator (Maybe APLAxis) APLArray
             | DyadicOp APLOperator (Maybe APLAxis) APLArray APLArray
             | ReduceOp APLOperator (Maybe APLAxis) APLArray
             | ProductOp APLOperator (Maybe APLOperator) APLArray APLArray
             deriving (Show, Data)

type APLArray = [APLElem]

pInt :: Parser Int
pInt = do char '-'
          negate <$> decimal
       <|> decimal

pAxis :: Parser APLAxis
pAxis = do char '['
           ns <- (:) <$> pInt <*> some (char ',' *> pInt)
           char ']'
           return $ Axis ns

pAPLOperator :: Parser APLOperator
pAPLOperator = (try (char '$') >> AntiOp <$> some alphaNumChar)
               <|> Op <$> some alphaNumChar

pAPLArg :: Parser APLElem
pAPLArg = try $ char '@' *> return Alpha
          <|> char '#' *> return Omega

pAPLNumber :: Parser APLElem
pAPLNumber = Number <$> pInt

pAPLMonadicOp :: Parser APLElem
pAPLMonadicOp = do name <- char '.' *> pAPLOperator
                   axis <- optional pAxis
                   space
                   MonadicOp name axis <$> pAPLArray

pAPLDyadicOp :: Parser APLElem
pAPLDyadicOp = do a1 <- pAPLArrayRecursionSafe
                  space
                  name <- char '.' *> pAPLOperator
                  axis <- optional pAxis
                  space
                  DyadicOp name axis a1 <$> pAPLArray

pAPLReduceOp :: Parser APLElem
pAPLReduceOp = do name <- char '.' *> pAPLOperator <* char '/'
                  space
                  ReduceOp name Nothing <$> pAPLArray

pAPLProductOp :: Parser APLElem
pAPLProductOp = do a1 <- pAPLArrayRecursionSafe
                   space
                   (name1, name2) <- pParen $ (,)
                     <$> pAPLOperator <* char '.'
                     <*> optional pAPLOperator
                   space
                   ProductOp name1 name2 a1 <$> pAPLArray

pAPLAntiElem :: Parser APLElem
pAPLAntiElem = do char '$'
                  AntiElem <$> some alphaNumChar

pAPLElem :: Parser APLElem
pAPLElem = try pAPLMonadicOp
           <|> try pAPLDyadicOp
           <|> try pAPLReduceOp
           <|> try pAPLProductOp
           <|> try pAPLAntiElem
           <|> try pAPLArg
           <|> try pAPLNumber

pArray :: Parser a -> Parser [a]
pArray p = (:) <$> p <*> many (char ',' *> p)

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

pAPLArrayRecursionSafe :: Parser APLArray
pAPLArrayRecursionSafe = try $ pParen pAPLArray
                         <|> pArray (try pAPLArg
                                     <|> pAPLAntiElem
                                     <|> pAPLNumber)

pAPLArray :: Parser APLArray
pAPLArray = pArray pAPLElem

testP        = parseTest pAPLArray ".hello 1,2,3"
testPMon     = parseTest pAPLArray ".hello[1,3] 1,2,3"
testPDy      = parseTest pAPLArray  ".ho 4,5,6 .hello 1,2,3"
testPDyRec   = parseTest pAPLArray  "3,4,5 .hello 1,2,3"
testPReduce  = parseTest pAPLArray ".add/ 1,2,3,4 .hello 1,@,4"
testPProd    = parseTest pAPLArray "3,4,5 (asdf.) 1,2,3"
testPAntiElm = parseTest pAPLArray "$test .plus 3"

testPFailure = parseTest pAPLArray "1 (.asd) 1"


hasAlpha :: APLArray -> Bool
hasAlpha arr = or $ rec <$> arr
  where
    rec Alpha = True
    rec (DyadicOp _ _ a1 a2) = hasAlpha a1 || hasAlpha a2
    rec (ProductOp _ _ a1 a2) = hasAlpha a1 || hasAlpha a2
    rec (MonadicOp _ _ a) = hasAlpha a
    rec (ReduceOp _ _ a) = hasAlpha a
    rec _ = False

hasOmega :: APLArray -> Bool
hasOmega arr = or $ rec <$> arr
  where
    rec Omega = True
    rec (DyadicOp _ _ a1 a2) = hasOmega a1 || hasOmega a2
    rec (ProductOp _ _ a1 a2) = hasOmega a1 || hasOmega a2
    rec (MonadicOp _ _ a) = hasOmega a
    rec (ReduceOp _ _ a) = hasOmega a
    rec _ = False

isDyadic :: APLArray -> Bool
isDyadic a = hasAlpha a && hasOmega a

isMonadic :: APLArray -> Bool
isMonadic a = not (hasAlpha a) && hasOmega a

mOps :: String -> TH.ExpQ
mOps "split"   = [| split |]
mOps "enclose" = [| enclose |]
mOps "fromEnum"= [| fmap fromEnum |]
mOps _         = [| id |]

dOps :: String -> TH.ExpQ
dOps "drop"        = [| purge |]
dOps "reshape"     = [| reshape |]
dOps "rotate"      = [| rotateFirst |]
dOps "rotateFirst" = [| rotate |]
dOps "plus"        = [| op (+) |]
dOps "minus"       = [| op (-) |]
dOps "mult"        = [| op (*) |]
dOps "div"         = [| op div |]
dOps "eq"          = [| op (\x y -> fromEnum $ x == y) |]
dOps "and"         = [| op andAPL |]
dOps "or"          = [| op orAPL |]
dOps _             = [| \x y -> y |]

monadicOp :: APLOperator -> TH.ExpQ
monadicOp (Op s) = mOps s
monadicOp (AntiOp s) = TH.varE (TH.mkName s)

dyadicOp :: APLOperator -> TH.ExpQ
dyadicOp (Op s) = dOps s
dyadicOp (AntiOp s) = TH.varE (TH.mkName s)

antiExprArr :: APLArray -> TH.ExpQ
antiExprArr arr
  | len == 1 = [| head $(mk_arr expl) |]
  | otherwise = [| Nest $ Array (V.fromList $(mk_arr expl)) [len] |]
  where
    expl = antiExprElem <$> arr
    len = length arr
    mk_arr [] = [| [] |]
    mk_arr (x:xs) = [| $x : $(mk_arr xs) |]

antiExprElem :: APLElem -> TH.ExpQ
antiExprElem Alpha = [| alpha |]
antiExprElem Omega = [| omega |]
antiExprElem (Number n) = [| Node n |]
antiExprElem (AntiElem n) = TH.varE (TH.mkName n)
antiExprElem (MonadicOp n _ arr) = [| $f $a |]
  where
    a = antiExprArr arr
    f = monadicOp n
antiExprElem (DyadicOp n _ arr1 arr2) = [| $f $a1 $a2 |]
  where
    a1 = antiExprArr arr1
    a2 = antiExprArr arr2
    f = dyadicOp n
antiExprElem (ReduceOp n _ arr) = [| reduce $f $a |]
  where
    a = antiExprArr arr
    f = dyadicOp n
antiExprElem (ProductOp n1 (Just n2) arr1 arr2)
  = [|innerProduct $f1 $f2 $a1 $a2 |]
  where
    f1 = dyadicOp n1
    f2 = dyadicOp n2
    a1 = antiExprArr arr1
    a2 = antiExprArr arr2
antiExprElem (ProductOp n1 Nothing arr1 arr2)
  = [|outerProduct $f1 $a1 $a2 |]
  where
    f1 = dyadicOp n1
    a1 = antiExprArr arr1
    a2 = antiExprArr arr2

antiExprExp :: APLArray -> Maybe TH.ExpQ
antiExprExp arr = return $ antiExprArr arr

quoteExprExp :: String -> TH.Q TH.Exp
quoteExprExp str = e >>= mkExpQ
  where
    e = case parse pAPLArray "" (T.pack str) of
                Left e -> fail $ show e
                Right a -> return a
    exq = dataToExpQ (const Nothing `extQ` antiExprExp)
    mkExpQ exp
      | isDyadic exp = [|\alpha omega -> $(exq exp)|]
      | isMonadic exp = [|\omega -> $(exq exp)|]
      | otherwise = exq exp

apl :: QuasiQuoter
apl = QuasiQuoter
    { quoteExp  = quoteExprExp
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

--[apl|1,0,-1 (rotateFirst.) 1,0,-1 (rotate.) .enclose #|]

{-
NOTE/TODO

Need to find a way to store APL operations of different types. For example "NestedArray Int -> NestedArray a -> NestedArray a"
and "NestedArray a -> NestedArray a -> NestedArray a" and so on.
These do not have to be taken care of during generation of Q Exp as they will be checked after the generation with Haskell type system.
Therefore, it has to be either somewhat put string as a function inside of Haskell QuasiQuote
or find a way to store different types of APL operations. Perhaps an extra level of abstraction via another type might work.

Anti-climatically, the solution was quite simple: making the "Ops table" return the ExpQ solved it. That way it does not have to check
type while building the expression. The Haskell Compiler will take care of types after building expression from the quasi quotes. 
-}

{-
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 
-}
 --board = [apl|5 7 .reshape 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0|]


-- life work!
-- life = [apl|1,# (or.and) 3,4 .eq .plus/ .plus/ 1,0,-1 (rotateFirst.) 1,0,-1 (rotate.) .enclose #|]

