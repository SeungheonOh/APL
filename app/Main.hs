{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified MyLib (someFunc)
import qualified Data.Vector as V
import Control.Exception (Exception, throwIO)
import GHC.Exception (throw, fromCallSiteList)
import Data.List
import Control.Applicative
import Control.Monad.Free

{-
A Breif Note about recursive strucutres
- Simple rose tree structure
-- data Tree a
--   = Leaf a
--   | Branch [Tree a]

- "Fix" can be used to construct a recursive strucure
-- data Fix f = Fix (f (Fix f))
-- data TreeF a r = LeafF a | BranchF [r]
-- test :: Fix (TreeF Int)
-- test = undefined

Because simpling putting its own type hinders one from functionally
interfacing with the nested data, it has to be done by recursive strucutres.

For example in non recursive structures,
-- Array (Array (Array ...))
the nested level can go on and there is no way to handle them all
-- f (Array vec ns) = ... 
Here we have no control of nested levels.
As we are not able to deconstruct the vector. :(

However in recursive structures, 
-- data NestedArray a
--   = Node a
--   | Nest (Array (NestedArray a))
--   deriving (Show)
the structure does the recursion.
-- f (Node a) = ...
-- f (Nest a) = ...
Here we have full control of nested levels. :)
-}



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
                  | InvalidAxisError
                  deriving (Show)

instance Exception APLException

data Array a = Array
  { _value :: V.Vector a
  , _shape :: [Int]
  } deriving (Eq, Show)

instance Functor Array where
  fmap f (Array vec ns) = Array (fmap f vec) ns

instance Applicative Array where
  pure a = Array (V.fromList [a]) [1]
  liftA2 f (Array va sa) (Array vb sb)
    = Array (V.zipWith f va vb) sh
    where
      sh = if length va > length vb then sa else sb

data NestedArray a
  = Node a
  | Nest (Array (NestedArray a))
  deriving (Show)

instance Functor NestedArray where
  fmap f (Node a) = Node $ f a
  fmap f (Nest arr) = Nest $ fmap (fmap f) arr

instance Applicative NestedArray where
  pure a = Node a
  Node f <*> a = fmap f a
  Nest f <*> a = Nest $ fmap (<*> a) f

test =  liftA2 (+) (Node 5) (fromList [Node 4, Node 3])
test2 = liftA2 (+) (iota 4) (iota 4)

fill :: NestedArray a -> NestedArray a
fill (Nest a) = Nest $ f a
  where
    f (Array vec ns)
      | length vec == targ = Array vec ns
      | otherwise = Array (V.take targ $ V.fromList [1..targ] >> vec) ns
      where
        targ = product ns
fill n = n

reshape :: [Int] -> NestedArray a -> NestedArray a
reshape r (Node a) = reshape r (Nest $ Array (V.fromList [Node a]) [1])
reshape r (Nest a) = fill $ Nest $ Array (_value a) r

iota :: Int -> NestedArray Int
iota a = fromList $ Node <$> [1..a]

enclose :: NestedArray a -> NestedArray a
enclose a = Nest $ Array (V.fromList [a]) [1]

first :: NestedArray a -> NestedArray a
first (Nest (Array vec _)) = vec V.! 0
first a = a

example :: NestedArray Int
example = reshape [3, 3] (iota 5)

example2 :: NestedArray Int
example2 = reshape [2, 2, 2] (enclose $ reshape [2, 2] (iota 5))

example3 :: NestedArray Int
example3 = reshape [3, 2] $ fromList [example, example2]

fromList :: [NestedArray a] -> NestedArray a
fromList l = Nest $ Array (V.fromList l) [length l]

fromA :: a -> NestedArray a
fromA v = fromList [Node v]

toList :: NestedArray a -> [a]
toList (Nest a) = concat $ toList <$> _value a
toList (Node a) = [a]

shape :: NestedArray a -> [Int]
shape (Nest (Array _ n)) = n
shape (Node _) = throw RankError -- this should not be evoked

value :: NestedArray a -> V.Vector (NestedArray a)
value (Nest arr) = _value arr
value (Node _) = throw RankError -- this should not be evoked

depth :: NestedArray a -> Int
depth = acc 0
  where
    acc d (Nest a) = maximum (acc (d+1) <$> V.toList (_value a))
    acc d (Node a) = d

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

convertDemention :: [Int] -> [Int] -> Int
convertDemention sha n = sum $ zipWith (*) mult $ subtract 1 <$> n
  where
    mult = fmap (product . ($ sha)) (lastN <$> reverse [1..(length sha -1)]) <> [1]

at :: NestedArray a -> [Int] -> NestedArray a
at (Nest (Array vec sh)) ns
  | any (<=0) ns          = throw IndexError
  | length ns > length sh = throw RankError
  | otherwise = Nest $ Array (V.slice (convertDemention sh ns) (product newShape) vec) newShape
  where
    s = drop (length ns) sh
    newShape = if null s then [1] else s
at a ns = a -- pattern for Node

pretty :: Show a => NestedArray a -> String
pretty a = p (depth a) a
  where
    title (Array _ ns) = intercalate "," $ show <$> ns
    extDem a = box (title a) $ intercalate "\n" $ pretty . at (Nest a) . (:[]) <$> [1..head$_shape a]
    p 1 (Nest a)
      | length ns == 1 = unwords $ V.toList $ fmap (fmtStr . pretty) vec
      | otherwise = extDem a
      where
        ns = _shape a
        vec = _value a
        longest = maximum $ length . pretty <$> vec
        fmtStr s = s ++ ([1..longest - length s] >> " ")
    p d (Nest a)
      | length ns <= 2 = chartWithTitle (title a) (last $ ns) (pretty <$> V.toList (_value a))
      | otherwise = extDem a
      where
        ns = _shape a
    p d (Node a) = show a

-- Returns given list without given index
beside :: Int -> [a] -> [a]
beside i a = take i a ++ drop (i+1) a

-- Generate indexs from given combinations
genIndex :: [[Int]] -> [Int] -> [[Int]]
genIndex opt arr
  | null opt = [arr]
  | otherwise = concat $ genIndex (tail opt) <$> ((arr ++) . (:[]) <$> head opt)

split :: Int -> NestedArray a -> NestedArray a
split ax (Nest (Array vec ns))
  | length ns < ax = throw InvalidAxisError
  | otherwise = reshape newshape $ fromList $ mk <$> genIndex req []
  where
    axis = ax - 1 -- 1 based indexing
    newshape = beside axis ns
    req = enumFromTo 1 <$> newshape
    mkInd i x = take axis i ++ [x] ++ drop axis i
    mk i = fromList $ (vec V.!) . convertDemention ns . mkInd i <$> [1..ns!!axis]
split _ a = a -- case for Node

-- apl drop
purge :: [Int] -> NestedArray a -> NestedArray a
purge d (Nest (Array vec ns))
  | length d > length ns = throw RankError
  | otherwise = undefined
purge _ a = a

op :: (a -> b -> c) -> NestedArray a -> NestedArray b -> NestedArray c
op f (Node a) b = fmap (f a) b
op f a (Node b) = fmap (`f` b) a
op f a b
  | shape a == [1]     = fromList $ op f (first a) . at b . (:[]) <$> [1..head $ shape b]
  | shape b == [1]     = fromList $ flip (op f) (first b) . at a . (:[]) <$> [1..head $ shape a]
  | shape a == shape b = Nest $ Array (V.zipWith (op f) (value a) (value b)) (shape a)
  | otherwise          = throw LengthError

atest :: NestedArray Float
atest = fmap (/2) (fromList [Node 2, Node 4, Node 8])


--btest = fmap (+(fromA 4)) (iota 5)
btest :: NestedArray Int
btest = fromA 5

main :: IO ()
main = undefined
