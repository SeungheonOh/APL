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
import qualified Data.Text as T
import Data.Void
import Control.Applicative hiding (many, some)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import Data.Generics.Aliases
import Data.Data
import Language.Haskell.TH (lamE)

type Parser = Parsec Void T.Text

data APLType = Number Int
             | Chars String
             deriving (Show, Data)

newtype APLExprAxis = Axis [Int] deriving (Show, Data)

data APLExpr = Alpha
             | Omega
             | Literal [Int]
             -- | Array (NestedArray APLExpr)
             | MonadicOp String (Maybe APLExprAxis) APLExpr
             | DyadicOp String (Maybe APLExprAxis) APLExpr APLExpr
             | ProductOp String (Maybe String) APLExpr APLExpr
             deriving (Show, Data)

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

pIntArray :: Parser [Int]
pIntArray = (:) <$> pInt <*> many (char ',' *> pInt)

pStringArray :: Parser [APLType]
pStringArray = (fmap.fmap) Chars ((:) <$> some alphaNumChar <*> some (char ',' *> some alphaNumChar))

pAxis :: Parser APLExprAxis
pAxis = do char '['
           ns <- (:) <$> pInt <*> some (char ' ' *> pInt)
           char ']'
           return $ Axis ns

pMonadicOp :: Parser APLExpr
pMonadicOp = do name <- char '.' *> some alphaNumChar 
                axis <- optional pAxis
                space
                MonadicOp name axis <$> (try pAPLExpr <|> pAPLTerm)

pDyadicOp :: Parser APLExpr
pDyadicOp = do a1 <- pAPLTerm
               space 
               name <- char '.' *> some alphaNumChar 
               axis <- optional pAxis
               space
               DyadicOp name axis a1 <$> (try pAPLExpr <|> pAPLTerm)

pProductOp :: Parser APLExpr
pProductOp = do a1 <- pAPLTerm
                space
                name1 <- char '(' *> some alphaNumChar
                name2 <- char '.' *> optional (some alphaNumChar) <* char ')'
                space
                ProductOp name1 name2 a1 <$> (try pAPLExpr <|> pAPLTerm)

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

pAPLTerm :: Parser APLExpr
pAPLTerm = try $ char '@' *> return Alpha
           <|> char '#' *> return Omega
           <|> Literal <$> pIntArray
           -- <|> Literal <$> pStringArray
           <|> pParen pAPLExpr

pAPLExpr :: Parser APLExpr
pAPLExpr = try pMonadicOp
           <|> try pProductOp
           <|> try pDyadicOp

pAPL :: Parser APLExpr
pAPL = try pAPLExpr <|> pAPLTerm 

test = parseTest pInt "-12345"
test2 = parseTest pInt "2345"
test3 = parseTest pIntArray "2"
test4 = parseTest pIntArray "2 3 -4 5"
test5 = parseTest pStringArray "hello world from APL EDSL"
test6 = parseTest pAPL ".ρ[1 2 3] .ρ[1 2 3] 1,2,3"
testa = parseTest pAPL ".o[1 2 3] (a .o a)"
test7 = parseTest pAPL ".ρ[1 2 3] (.ρ[1 2 3] a)"
test8 = parseTest pAPL ".ρ[1 2 3] (.ρ[1 2 3] a a)"
testb :: IO ()
testb = parseTest pAPL ".ρ[1 2 3] @"
testc = parseTest pAPL ".ρ[1 2 3] (@ .reshape[1 2 3] 1,2,3,4,5)"
testd = parseTest pAPL "hello,world .ρ .z 1,2,3,45"
teste = parseTest pAPL ".ρ .z 1,2,3,4"
testf = parseTest pAPL "3,4⍴3,4,5,6,7,2,3,4,5,"
testg = parseTest pMonadicOp ".enclose 3,3 .reshape 0,0,0,0,1,0,0,0,0"
testga = parseTest pAPL ".enclose 3,3 .reshape 0,0,0,0,1,0,0,0,0"
testproduct = parseTest pAPL "1,2,3,4 (a.b) 3,4"
testh = parseTest pAPL "1,0,-1 (rotate.) .enclose #"

hasAlpha :: APLExpr -> Bool
hasAlpha Alpha = True
hasAlpha (DyadicOp _ _ a1 a2) = hasAlpha a1 || hasAlpha a2
hasAlpha (ProductOp _ _ a1 a2) = hasAlpha a1 || hasAlpha a2
hasAlpha (MonadicOp _ _ a) = hasAlpha a
hasAlpha _ = False

hasOmega :: APLExpr -> Bool
hasOmega Omega = True
hasOmega (DyadicOp _ _ a1 a2) = hasOmega a1 || hasOmega a2
hasOmega (ProductOp _ _ a1 a2) = hasOmega a1 || hasOmega a2
hasOmega (MonadicOp _ _ a) = hasOmega a
hasOmega _ = False

isDyadic :: APLExpr -> Bool
isDyadic a = hasAlpha a && hasOmega a

isMonadic :: APLExpr -> Bool
isMonadic a = not (hasAlpha a) && hasOmega a

monadicOps :: String -> TH.ExpQ 
monadicOps "split"   = [| split |]
monadicOps "enclose" = [| enclose |]
monadicOps _         = [| id |]

dyadicOps :: String -> TH.ExpQ 
dyadicOps "drop"        = [| purge |]
dyadicOps "reshape"     = [| reshape |] 
dyadicOps "rotate"      = [| rotateFirst |]
dyadicOps "rotateFirst" = [| rotate |]
dyadicOps "plus"        = [| op (+) |]
dyadicOps "minus"       = [| op (-) |]
dyadicOps "mult"        = [| op (*) |]
dyadicOps "eq"          = [| op (==) |]
dyadicOps _             = [| \x y -> y |]

antiExprExp :: APLExpr -> Maybe (TH.Q TH.Exp)
antiExprExp Alpha = Just [| alpha |]
antiExprExp Omega = Just [| omega |]
antiExprExp (MonadicOp n axis a) = do
  na <- antiExprExp a
  return [|$f $na|]
    where
      f = monadicOps n 
antiExprExp (DyadicOp n axis a1 a2) = do
  na1 <- antiExprExp a1
  na2 <- antiExprExp a2
  return [|$f $na1 $na2|]
    where
      f = dyadicOps n
antiExprExp (ProductOp n1 (Just n2) a1 a2) = do
  na1 <- antiExprExp a1
  na2 <- antiExprExp a2
  return [|innerProduct $op1 $op2 $na1 $na2|]
    where
      op1 = dyadicOps n1
      op2 = dyadicOps n2
antiExprExp (ProductOp n1 Nothing a1 a2) = do
  na1 <- antiExprExp a1
  na2 <- antiExprExp a2
  return [|outerProduct $op1 $na1 $na2|]
    where
      op1 = dyadicOps n1
antiExprExp (Literal l) = Just [| fromList l |]

quoteExprExp :: String -> TH.Q TH.Exp
quoteExprExp str = e >>= mkExpQ
  where
    e = case parse pAPL "" (T.pack str) of
                Left e -> fail $ show e
                Right a -> return a
    exq a = dataToExpQ (const Nothing `extQ` antiExprExp) a
    mkExpQ exp
      | isDyadic exp = lamE [TH.varP (TH.mkName "alpha"), TH.varP (TH.mkName "omega")] $ exq exp
      | isMonadic exp = lamE [TH.varP (TH.mkName "omega")] $ exq exp
      -- | isDyadic exp = [|\a w -> $(exq exp)|]
      -- | isMonadic exp = [|\w -> $(exq exp)|]
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
