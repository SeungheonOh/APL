{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.APL
rboard :: [[Int]]
rboard = [
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
  [0, 0, 0, 0, 1, 1, 1, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
-- rboard = [
--   [0, 0, 0, 0, 0, 0, 0],
--   [0, 0, 0, 0, 0, 0, 0],
--   [0, 0, 0, 0, 0, 0, 0],
--   [0, 0, 0, 1, 0, 0, 0],
--   [0, 0, 0, 0, 0, 0, 0],
--   [0, 0, 0, 0, 0, 0, 0],
--   [0, 0, 0, 0, 0, 0, 0]]

board :: NestedArray Int
board = reshape (fromList [length rboard, length $ head rboard]) $ fromList $ concat rboard

<<<<<<< HEAD
main :: IO ()
main = do
  p $ life board
  putStrLn "Hello, Haskell!"

aAnd :: Int -> Int -> Int
aAnd 1 1 = 1
aAnd 1 0 = 0
aAnd 0 1 = 0
aAnd 0 0 = 0

aOr :: Int -> Int -> Int
aOr 1 1 = 1
aOr 1 0 = 1
aOr 0 1 = 1
aOr 0 0 = 0

aaor = op aOr
aaand = op aAnd


toInt = fmap fromEnum

life = [apl|1,# ($aaor.$aaand) .$toInt 3,4 .eq .plus/ .plus/ 1,0,-1 (rotateFirst.) 1,0,-1 (rotate.) .enclose #|]
