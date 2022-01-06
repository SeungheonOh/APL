{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.APL.Parser where

import Data.APL.Array
import Data.APL.Operator

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Data
import Data.Void
import Control.Applicative hiding (many, some)

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

