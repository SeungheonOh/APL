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
pMonadicOp = MonadicOp
          <$> (char '.' *> some alphaNumChar)
          <*> optional pAxis <* char ' '
          <*> (try pAPLExpr <|> pAPLTerm)

pDyadicOp :: Parser APLExpr
pDyadicOp = do a1 <- pAPLTerm <* char ' '
               name <- char '.' *> some alphaNumChar
               axis <- optional pAxis <* char ' '
               DyadicOp name axis a1 <$> (try pAPLExpr <|> pAPLTerm)

pParen :: Parser a -> Parser a
pParen p = char '(' *> p <* char ')'

pAPLTerm :: Parser APLExpr
pAPLTerm = try $ char '@' *> return Alpha
           <|> char '#' *> return Omega
           <|> Literal <$> pIntArray
           -- <|> Literal <$> pStringArray
           <|> pParen pAPLExpr

pAPLExpr :: Parser APLExpr
pAPLExpr = try pDyadicOp
           <|> pMonadicOp

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
testb = parseTest pAPL ".ρ[1 2 3] @"
testc = parseTest pAPL ".ρ[1 2 3] (@ .reshape[1 2 3] 1,2,3,4,5)"
testd = parseTest pAPL "hello,world .ρ .z 1,2,3,45"
teste = parseTest pAPL ".ρ .z 1,2,3,4"
testf = parseTest pAPL "1,1 .rotate 3,3 .reshape 1,2,3,4,5,6,7,8"

-- prog :: QuasiQuoter
-- prog = QuasiQuoter {
--       quoteExp = \str -> do
--         l <- TH.location
--         let pos = (TH.loc_filename l,
--                    fst (TH.loc_start l),
--                    snd (TH.loc_start l))
--         c <- parse pAPLExpr str
--         dataToExpQ (const Nothing) c
--     , quotePat  = undefined
--     , quoteType = undefined
--     , quoteDec  = undefined
--     }

hasAlpha :: APLExpr -> Bool
hasAlpha Alpha = True
hasAlpha (DyadicOp _ _ a1 a2) = hasAlpha a1 || hasAlpha a2
hasAlpha (MonadicOp _ _ a) = hasAlpha a
hasAlpha _ = False

hasOmega :: APLExpr -> Bool
hasOmega Omega = True
hasOmega (DyadicOp _ _ a1 a2) = hasOmega a1 || hasOmega a2
hasOmega (MonadicOp _ _ a) = hasOmega a
hasOmega _ = False

isDyadic :: APLExpr -> Bool
isDyadic a = hasAlpha a && hasOmega a

isMonadic :: APLExpr -> Bool
isMonadic a = not (hasAlpha a) && hasOmega a

monadicOps :: String -> (NestedArray a -> NestedArray a)
monadicOps "split" = split
monadicOps _ = id

dyadicOps :: String -> (NestedArray Int -> NestedArray a -> NestedArray a)
dyadicOps "drop" = purge
dyadicOps "reshape" = reshape
dyadicOps "rotate" = rotateFirst
dyadicOps _ = \x y -> y

antiExprExp :: APLExpr -> Maybe (TH.Q TH.Exp)
antiExprExp Alpha = Just $ [| a |]
antiExprExp Omega = Just $ [| w |]
antiExprExp (MonadicOp n axis a) = do
  na <- antiExprExp a
  return $ [|$f $na|]
    where
      f = [| monadicOps n |]
antiExprExp (DyadicOp n axis a1 a2) = do
  na1 <- antiExprExp a1
  na2 <- antiExprExp a2
  return $ [|$f $na1 $na2|]
    where
      f = [| dyadicOps n |]
antiExprExp (Literal l) = Just $ [| fromList l |]

quoteExprExp :: String -> TH.Q TH.Exp
quoteExprExp str = do
          e >>= mkExpQ
  where
    e = case parse pAPL "" (T.pack str) of
                Left e -> fail $ show e
                Right a -> return a
    mkExpQ a
      | isDyadic a = lamE [TH.varP (TH.mkName "a"), TH.varP (TH.mkName "w")] $ dataToExpQ (const Nothing `extQ` antiExprExp) a
      | isMonadic a = lamE [TH.varP (TH.mkName "w")] $ dataToExpQ (const Nothing `extQ` antiExprExp) a
      | otherwise = dataToExpQ (const Nothing `extQ` antiExprExp) a

apl :: QuasiQuoter
apl = QuasiQuoter
    { quoteExp  = quoteExprExp
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
