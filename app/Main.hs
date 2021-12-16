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
    strs = lines <$> s ++ ntimes (length s * (col-1)`mod`col) [""]
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
                  | Debug
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
  fmap f (Nest arr)
    | _shape arr == [1] = fmap f (first $ Nest arr) -- Reduce singletons
    | otherwise = Nest $ fmap (fmap f) arr

instance Applicative NestedArray where
  pure a = Node a
  Node f <*> a = fmap f a
  Nest f <*> a = Nest $ fmap (<*> a) f

test =  liftA2 (+) (Node 5) (fromList [4, 3])
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

fillWith :: NestedArray a -> NestedArray a -> NestedArray a
fillWith r (Nest a) = Nest $ f a
  where
    f (Array vec ns)
      | length vec == targ = Array vec ns
      | otherwise = Array (V.take targ $ vec V.++ V.fromList ([1..targ] >> [r])) ns
      where
        targ = product ns
fillWith _ n = n

reshape :: NestedArray Int -> NestedArray a -> NestedArray a
reshape r a
  | length (shape r) /= 1 = throw RankError
  | otherwise = f (toList r) a
    where
      f r (Node a) = f r (Nest $ Array (V.fromList [Node a]) [1])
      f r (Nest a) = fill $ Nest $ Array (_value a) r

reshapeWith :: NestedArray Int -> NestedArray a -> NestedArray a -> NestedArray a
reshapeWith rp r a
  | length (shape rp) /= 1 = throw RankError
  | otherwise = f (toList rp) r a
    where
      f rp r (Node a) = f rp r (Nest $ Array (V.fromList [Node a]) [1])
      f rp r (Nest a) = fillWith r $ Nest $ Array (_value a) rp


iota :: Int -> NestedArray Int
iota a = nest $ Node <$> [1..a]

enclose :: NestedArray a -> NestedArray a
enclose a = Nest $ Array (V.fromList [a]) [1]

first :: NestedArray a -> NestedArray a
first (Nest (Array vec _)) = vec V.! 0
first a = a

example :: NestedArray Int
example = reshape (fromList [3, 3]) (iota 5)

example2 :: NestedArray Int
example2 = reshape (fromList [2, 2, 2]) (enclose $ reshape (fromList [2, 2]) (iota 5))

example3 :: NestedArray Int
example3 = reshape (fromList [3, 2]) $ nest [example, example2]

nest :: [NestedArray a] -> NestedArray a
nest l = Nest $ Array (V.fromList l) [length l]

fromList :: [a] -> NestedArray a
fromList l = Nest $ Array (V.fromList $ Node <$> l) [length l]

fromA :: a -> NestedArray a
fromA v = Nest $ Array (V.fromList [Node v]) [1]

toList :: NestedArray a -> [a]
toList (Nest a) = concat $ toList <$> _value a
toList (Node a) = [a]

shape :: NestedArray a -> [Int]
shape (Nest (Array _ n)) = n
shape (Node _) = [1]

value :: NestedArray a -> V.Vector (NestedArray a)
value (Nest arr) = _value arr
value (Node v) = V.fromList [Node v]

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
  | newShape == [1]       = vec V.! ind -- prevent useless nests
  | otherwise = Nest $ Array (V.slice ind (product newShape) vec) newShape
  where
    s = drop (length ns) sh
    newShape = if null s then [1] else s
    ind = convertDemention sh ns
at a ns = a -- pattern for Node

pretty :: Show a => NestedArray a -> String
pretty a
  | 0 `elem` shape a = "EMPTY"
  | otherwise = p (depth a) a
  where
    title (Array _ ns) = intercalate "," $ show <$> ns
    extDem a = box (title a) $ intercalate "\n" $ pretty . at (Nest a) . (:[]) <$> [1..head$_shape a]
    fmtStr w s = s ++ ([1..w - length s] >> " ")
    mkLine w (Nest a) = unwords $ V.toList $ fmap (fmtStr w . pretty) (_value a)
    mkLine _ (Node a) = show a
    p 1 (Nest a)
      | length ns == 1 = mkLine d1Max na
      | length ns == 2 = box (title a) $ intercalate "\n" $ mkLine d2Max <$> d2
      | otherwise = extDem a
      where
        na = Nest a
        ns = _shape a
        lineMax a = maximum $ length . pretty <$> value a
        d1Max = lineMax na
        d2 = at na . (:[]) <$> [1..head$shape na]
        d2Max = maximum $ lineMax <$> d2
    p d (Nest a)
      | length ns <= 2 = chartWithTitle (title a) (last ns) (pretty <$> V.toList (_value a))
      | otherwise = extDem a
      where
        ns = _shape a
    p d (Node a) = show a

reduce :: (NestedArray a -> NestedArray a -> NestedArray a) -> NestedArray a -> NestedArray a
reduce f a = foldr1 f $ value a

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
  | otherwise = reshape (fromList newshape) $ nest $ mk <$> genIndex req []
  where
    axis = ax - 1 -- 1 based indexing
    newshape = beside axis ns
    req = enumFromTo 1 <$> newshape
    mkInd i x = take axis i ++ [x] ++ drop axis i
    mk i = nest $ (vec V.!) . convertDemention ns . mkInd i <$> [1..ns!!axis]
split _ a = a -- case for Node

-- apl drop
purge :: NestedArray Int -> NestedArray a -> NestedArray a
purge d (Nest a)
  | length (shape d) /= 1               = throw RankError
  | head (shape d)  > length (_shape a) = throw RankError
  | otherwise = reshape newshape $ nest $ at (Nest a) <$> genIndex req []
  where
    rank = length $ _shape a
    diff = reshapeWith (fromList [rank]) (Node 0) d
    lim x = if x < 0 then 0 else x
    newshape = lim <$> op (-) (fromList $ _shape a) (abs <$> diff)
    mkInd a b
      | b >= 0 = [1+b..a]
      | otherwise = [1..a+b]
    req = toList $ op mkInd (fromList $ _shape a) diff
purge _ a = a

rotate :: NestedArray Int -> NestedArray a -> NestedArray a
rotate d a
  | length (shape d) /= 1              = throw RankError
  | head (shape d) > length (shape a) = throw RankError
  | otherwise = reshape newshape $ nest $ at a <$> genIndex req []
    where
      rank = length $ shape a
      newshape = fromList $ shape a
      rotFac = reshapeWith (fromList [rank]) (Node 0) d
      rot :: [Int] -> Int -> [Int]
      rot xs n = take lxs . drop (n `mod` lxs) . cycle $ xs
        where lxs = length xs
      req = toList $ op (rot . enumFromTo 1) (fromList $ shape a) rotFac

innerProduct :: Show c =>
                (NestedArray c -> NestedArray c -> NestedArray c)
             -> (NestedArray a -> NestedArray b -> NestedArray c)
             -> NestedArray a
             -> NestedArray b
             -> NestedArray c
innerProduct o1 o2 a b
  | head (shape a) == 1 = reduce o1 $ o2 a b
  | head (shape b) == 1 = reduce o1 $ o2 a b
  | last (shape a) /= head (shape b) = throw LengthError
  | null (value newshape) = foldr1 o1 $ V.zipWith o2 (value a) (value b)
  | otherwise = reshape newshape $ nest $ mk <$> genIndex req []
  where
    overlapped = [1..head (shape b)]
    newshape = fromList $ init (shape a) ++ tail (shape b)
    req = enumFromTo 1 <$> toList newshape
    mkL i = at a . (\x->take (length (shape a) - 1) i ++ [x]) <$> overlapped
    mkR i = at b . (\x->x : lastN (length (shape b) - 1) i) <$> overlapped
    mk  i = foldr1 o1 $ zipWith o2 (mkL i) (mkR i)

op :: (a -> b -> c) -> NestedArray a -> NestedArray b -> NestedArray c
op f (Node a) b = fmap (f a) b
op f a (Node b) = fmap (`f` b) a
op f a b
  | shape a == [1]     = Nest $ Array (op f (first a) <$> value b)        $ shape b
  | shape b == [1]     = Nest $ Array (flip (op f) (first b) <$> value a) $ shape a
  | shape a == shape b = Nest $ Array (V.zipWith (op f) (value a) (value b)) (shape a)
  | otherwise          = throw LengthError

main :: IO ()
main = undefined

-- putStrLn $ pretty $ op (+) (reshape [3, 3] $ iota 3) (enclose $ reshape [3,3] $ iota 5)
p :: Show a => NestedArray a -> IO ()
p a = putStrLn $ pretty a
