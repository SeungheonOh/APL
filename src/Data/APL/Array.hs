module Data.APL.Array where

import qualified Data.Vector as V

import Control.Applicative

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

shape :: NestedArray a -> [Int]
shape (Nest (Array _ n)) = n
shape (Node _) = [1]

value :: NestedArray a -> V.Vector (NestedArray a)
value (Nest arr) = _value arr
value (Node v) = V.fromList [Node v]


first :: NestedArray a -> NestedArray a
first (Nest (Array vec _)) = vec V.! 0
first a = a

fromList :: [a] -> NestedArray a
fromList l = Nest $ Array (V.fromList $ Node <$> l) [length l]

fromA :: a -> NestedArray a
fromA v = Nest $ Array (V.fromList [Node v]) [1]

toList :: NestedArray a -> [a]
toList (Nest a) = concat $ toList <$> _value a
toList (Node a) = [a]
