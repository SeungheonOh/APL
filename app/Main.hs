{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified MyLib (someFunc)
import qualified Data.Vector as V
import Control.Exception (Exception, throwIO)
import GHC.Exception (throw)
import Data.List
import Control.Applicative


chartWithTitle :: String -> Int -> [String] -> String
chartWithTitle title col s = top ++ mid ++ bot
  where
    ntimes n s = [1..n] >> s
    strs = lines <$> (s ++ ntimes ((length s * (col-1))`mod`col) [""])
    maxCol = maximum $ length <$> concat strs
    maxRow = maximum $ length <$> strs
    fmtStr s = ntimes (maxCol - length s) " " ++ s
    eqlines a = (\x -> x ++ ([1..maximum (length <$> a)- length x] >> [""])) <$> a
    perline a = (\x -> (!! x) <$> a) <$> [0..length (head a)-1]
    eachCols a
      | null a = []
      | otherwise = [perline (eqlines $ take col a)] <> eachCols (drop col a)
    mkSep a b c d n = a
      ++ (d ++ ntimes (maxCol - length d) "─")
      ++ ntimes (col-1) (b ++ ntimes maxCol "─")
      ++ c ++ n
    top = mkSep "┌" "┬" "┐" title "\n"
    sep = mkSep "├" "┼" "┤" ""    "\n"
    bot = mkSep "└" "┴" "┘" ""    ""
    dataLine a = (\x -> "│" ++ intercalate "│" x ++ "│") . fmap fmtStr <$> a
    mid = intercalate sep $ (\x -> intercalate "\n" x ++ "\n") . dataLine <$> eachCols strs

chart :: Int -> [String] -> String
chart = chartWithTitle ""

box :: String -> String -> String
box title s = chartWithTitle title 1 [s]

data APLException = RankError
                  | IndexError
                  | LengthError
                  deriving (Show)

instance Exception APLException

data Array a = Array
  { value :: V.Vector a
  , shape :: [Int]
  } deriving (Eq)

instance Functor Array where
  fmap f arr = Array (fmap f (value arr)) (shape arr)

instance Show a => Show (Array a) where
  show arr
    | length ns == 1 = unwords $ V.toList $ fmap (fmtStr . show) vec
    | otherwise = box title $ intercalate "\n" $ show . at arr . (:[]) <$> [1..head ns]
      where
        ns = shape arr
        vec = value arr
        longest = maximum $ length . show <$> V.toList vec
        fmtStr s = s ++ ([1..longest - length s] >> " ")
        title = intercalate "," $ show <$> ns

instance Show a => Show (Array (Array a)) where
  show arr
    | length ns <= 2 = chartWithTitle title (head ns) $ show <$> V.toList vec
    | otherwise = box title $ intercalate "\n" $ show . at arr . (:[]) <$> [1..head ns]
      where
        ns = shape arr
        vec = value arr
        title = intercalate "," $ show <$> ns

empty :: Array a
empty = Array V.empty []

fill :: Array a -> Array a
fill (Array vec ns)
  | length vec == targ = Array vec ns
  | otherwise = Array (V.take targ $ V.fromList [1..targ] >> vec) ns -- Yes this is terrible
  where
    targ = product ns

reshape :: [Int] -> [a] -> Array a
reshape a d = fill $ Array (V.fromList d) a

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

at :: Array a -> [Int] -> Array a
at (Array vec ns) n
  | any (<= 0) n = throw IndexError
  | otherwise = Array (V.slice ind (product shape) vec) shape
  where
    mult = fmap (product . ($ ns)) (lastN <$> reverse [1..(length ns - 1)]) <> [1]
    ind = sum $ zipWith (*) mult $ subtract 1 <$> n
    sh = drop (length n) ns
    shape
      | null sh = [1]
      | otherwise = sh

singleton :: Array a -> Maybe a
singleton (Array vec ns)
  | length ns == 1 && head ns == 1 = Just $ V.head vec
  | otherwise = Nothing

main :: IO ()
main = do
  print $ reshape [2, 4] [1..8]
  print $ singleton $ at (reshape [2, 4] [1..8]) [2, 2]
  print $ reshape [2, 2] [reshape [2, 2] [1, 2, 4, 5]]
