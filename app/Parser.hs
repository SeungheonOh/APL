{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Main ( NestedArray, fromList )

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
import Control.Applicative hiding (many, some)

type Parser = Parsec Void Text

data APLType = Number Int
             | Chars String 
             deriving (Show)

newtype APLExprAxis = Axis [Int] deriving (Show)

data APLExpr = Alpha
             | Omega
             | Literal (NestedArray APLType)
             | Array (NestedArray APLExpr)
             | MonadicOp String (Maybe APLExprAxis) APLExpr
             | DyadicOp String (Maybe APLExprAxis) APLExpr APLExpr
             deriving (Show)

pNat :: Parser Int
pNat = do x <- many digitChar
          return $ read x
              
pInt :: Parser Int
pInt = do char '-'
          negate <$> pNat
       <|> pNat

pAPLType :: Parser APLType
pAPLType = do char '"'
              s <- some alphaNumChar
              char '"'
              return $ Chars s
           <|> (Number <$> pInt)

pIntArray :: Parser (NestedArray APLType)
pIntArray = fromList <$> (fmap.fmap) Number ((:) <$> pInt <*> some (char ',' *> pInt))

pStringArray :: Parser (NestedArray String)
pStringArray = fromList <$> ((:) <$> some alphaNumChar <*> some (char ' ' *> some alphaNumChar))

pAxis :: Parser APLExprAxis
pAxis = do char '['
           ns <- (:) <$> pInt <*> some (char ' ' *> pInt)
           char ']'
           return $ Axis ns

pMonadicOp :: Parser APLExpr
pMonadicOp = MonadicOp
          <$> (char '.' *> some alphaNumChar)
          <*> optional pAxis <* char ' '
          <*> try (pAPLTerm <|> pAPLExpr)

pDyadicOp :: Parser APLExpr
pDyadicOp = do a1 <- pAPLTerm <* char ' '
               name <- (char '.' *> some alphaNumChar)
               axis <- optional pAxis <* char ' '
               a2 <- (pAPLTerm <|> pAPLExpr)
               return $ DyadicOp name axis a1 a2

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

pAPLTerm :: Parser APLExpr
pAPLTerm = try $ char 'a' *> return Alpha
           <|> char 'w' *> return Omega
           <|> Literal <$> pIntArray
           <|> pParen pAPLExpr
  
pAPLExpr :: Parser APLExpr
pAPLExpr = try pDyadicOp
           <|> pMonadicOp

test = parseTest pInt "-12345"
test2 = parseTest pInt "2345"
test3 = parseTest pIntArray "2 3 4 5 asdf"
test4 = parseTest pIntArray "2 3 -4 5"
test5 = parseTest pStringArray "hello world from APL EDSL"
test6 = parseTest pAPLExpr "ρ[1 2 3] a ρ[1 2 3] a a"
testa = parseTest pAPLExpr "o[1 2 3] (a o a)"
test7 = parseTest pAPLExpr "ρ[1 2 3] (ρ[1 2 3] a)"
test8 = parseTest pAPLExpr "ρ[1 2 3] (ρ[1 2 3] a a)"
testb = parseTest pAPLExpr "ρ[1 2 3] a"
testc = parseTest pAPLExpr ".ρ[1 2 3] (a .reshape[1 2 3] 1 2 3 4 5)"
testd = parseTest pAPLExpr "w .ρ .z 1,2,3,45"
