{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified MyLib (someFunc)
import qualified Data.Vector as V
import Control.Exception (Exception, throwIO)
import GHC.Exception (throw)
import Data.List
import Control.Applicative
import Control.Monad.Free


chartWithTitle :: String -> Int -> [String] -> String
chartWithTitle title col s = top ++ mid ++ bot
  where
    ntimes n s = [1..n] >> s
    strs = lines <$> s ++ ntimes ((length s * (col-1))`mod`col) [""]
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
  show _ = "nope"
  -- show arr
  --   | length ns == 1 = unwords $ V.toList $ fmap (fmtStr . show) vec
  --   | otherwise = box title $ intercalate "\n" $ show . at arr . (:[]) <$> [1..head ns]
  --     where
  --       ns = shape arr
  --       vec = value arr
  --       longest = maximum $ length . show <$> V.toList vec
  --       fmtStr s = s ++ ([1..longest - length s] >> " ")
  --       title = intercalate "," $ show <$> ns

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

iota :: Int -> [Int]
iota a = [1..a]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

convertDemention :: [Int] -> [Int] -> Int
convertDemention sha n = sum $ zipWith (*) mult $ subtract 1 <$> n
  where
    mult = fmap (product . ($ sha)) (lastN <$> reverse [1..(length sha -1)]) <> [1]

-- at :: Array a -> [Int] -> Array a
-- at (Array vec ns) n
--   | any (<= 0) n = throw IndexError
--   | otherwise = Array (V.slice ind (product shape) vec) shape
--   where
--     ind = convertDemention ns n
--     sh = drop (length n) ns
--     shape = if null sh then [1] else sh

atEither :: Array a -> [Int] -> Either (Array a) a
atEither (Array vec ns) n
  | any (<= 0) n = throw IndexError
  | shape == [1] = Right $ vec V.! ind
  | otherwise = Left $ Array (V.slice ind (product shape) vec) shape
  where
    ind = convertDemention ns n
    sh = drop (length n) ns
    shape = if null sh then [1] else sh

singleton :: Array a -> Maybe a
singleton (Array vec ns)
  | length ns == 1 && head ns == 1 = Just $ V.head vec
  | otherwise = Nothing

-- Returns given list without given index
beside :: Int -> [a] -> [a]
beside i a = take i a ++ drop (i+1) a

-- Generate indexs from given combinations
genIndex :: [[Int]] -> [Int] -> [[Int]]
genIndex opt arr
  | null opt = [arr]
  | otherwise = concat $ genIndex (tail opt) <$> ((arr ++) . (:[]) <$> head opt)

-- split :: Show a => Array a -> Int -> IO() --Array a
split :: Int -> Array a -> Array (Array a)
split a arr
  | length ns == 1 = Array (V.fromList [arr]) [1]
  | otherwise = Array (V.fromList $ mk <$> genIndex ((\x->[1..x]) <$> newshape) []) newshape
    where
      vec = value arr
      ns = shape arr
      axis = a - 1 -- 1 based indexing
      newshape = beside axis (shape arr)
      mk i = Array (V.fromList $ (\x -> vec V.! convertDemention ns (take axis i ++ [x] ++ drop axis i)) <$> [1..ns!!axis]) [ns!!axis]

main :: IO ()
main = do
  -- print $ reshape [2, 4] [1..8]
  print $ reshape [2, 4] (iota 5)
  print $ reshape [2, 4] [reshape [2, 4] [1..8]]
  -- print $ singleton $ at (reshape [2, 4] [1..8]) [2, 2]
  -- print $ reshape [2, 2] [reshape [2, 2] [1, 2, 4, 5]]
  --split (reshape [3,3,3] (iota 9)) 1
  print $ reshape [3,3,3] (iota 9)
  print $ split 1 (reshape [3,4,5] (iota 9))
  print $ split 1 (reshape [3] (iota 9))
  --print $ split 2 $ reshape [2, 4] [reshape [2, 4] (iota 8)]
  print $ reshape [2, 4] [reshape [2, 4] (iota 8)]
  print $ reshape [2, 3] [reshape [2, 4] [reshape [2, 4] (iota 88888)]]
  print $ reshape [2, 4] [reshape [2, 4] (iota 88888)]
  --print $ split (reshape [3,3] [1..9]) 2

-- data Tree a
--   = Leaf a
--   | Branch [Tree a]

-- data Fix f = Fix (f (Fix f))
-- data TreeF a r = LeafF a | BranchF [r]

-- test :: Fix (TreeF Int)
-- test = undefined

data NestedArray a
  = Node a
  | Nest (Array (NestedArray a))

instance Functor NestedArray where
  fmap f (Node a) = Node $ f a
  fmap f (Nest arr) = Nest $ fmap (fmap f) arr

example :: NestedArray Int
example = Nest (reshape [3, 3] [Node 5, Node 3, Node 5, Node 2])

example2 :: NestedArray Int
example2 = Nest (reshape [2, 2, 2] [Nest(reshape [2, 2] [Node 5])])

example3 :: NestedArray Int
example3 = Nest (reshape [2] [Nest(reshape [3, 3, 3] [Nest(reshape [3, 3] [Node 5])])])

fromList :: [a] -> NestedArray a
fromList l = Nest $ Array (V.fromList $ Node <$> l) [length l]

toList :: NestedArray a -> [a]
toList (Nest a) = concat $ toList <$> value a
toList (Node a) = [a]
  
depth :: NestedArray a -> Int
depth = acc 0
  where
    acc d (Nest a) = maximum (acc (d+1) <$> V.toList (value a))
    acc d (Node a) = d

instance Show a => Show (NestedArray a) where
  show a = show' (depth a) a
    where
      show' 1 (Nest a)
        | length ns == 1 = unwords $ V.toList $ fmap (fmtStr . show) vec
        | otherwise = box title $ intercalate "\n" $ show . at (Nest a) . (:[]) <$> [1..head ns]
        where
          ns = shape a
          vec = value a
          longest = maximum $ length . show <$> vec
          fmtStr s = s ++ ([1..longest - length s] >> " ")
          title = intercalate "," $ show <$> ns
      show' d (Nest a)
        | length ns <= 2 = chartWithTitle title (head ns) (show' (d-1) <$> V.toList (value a)) 
        | otherwise = box title $ intercalate "\n" $ show . at (Nest a) . (:[]) <$> [1..head ns]
        where
          ns = shape a
          title = intercalate "," $ show <$> ns
      show' d (Node a) = show a

at :: NestedArray a -> [Int] -> NestedArray a
at (Node a) ns = Node a
at (Nest a) ns = Nest $ Array (V.slice (convertDemention sh ns) (product newShape) vec) newShape
  where
    vec = value a
    sh = shape a
    newShape = if null (drop (length ns) sh) then [1] else drop (length ns) sh

cat :: Array a -> [Int] -> Array a
cat (Array vec ns) n
  | any (<= 0) n = throw IndexError
  | otherwise = Array (V.slice ind (product shape) vec) shape
  where
    ind = convertDemention ns n
    sh = drop (length n) ns
    shape = if null sh then [1] else sh
