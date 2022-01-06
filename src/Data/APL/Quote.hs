{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.APL.Quote where

import Data.APL.Parser

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH
import Data.Generics.Aliases

import Text.Megaparsec

import Data.Data
import qualified Data.Vector as V
import qualified Data.Text as T

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
