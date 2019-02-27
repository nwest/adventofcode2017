{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.List (foldl')
import Control.Arrow

import qualified Data.Map as M hiding (Map)
import Data.Map (Map)

import Text.Printf
import Data.Char (ord)
import Data.Bits (xor)


main :: IO ()
main = numberNine

captcha :: [Int] -> Int
captcha s = let fixedInput = reverse (head s : reverse s)
                pairs = concatMap (by 2) [fixedInput, tail fixedInput]
            in
              sum . map matchingCount $ pairs
  where
    matchingCount [a, b] = if a == b then a else 0
    matchingCount _ = 0

digitPairs :: [a] -> [(a, a)]
digitPairs xs = let step = length xs `div` 2
                in f xs step (cycle xs)
  where 
    f [] _ _ = []
    f (x':xs') s r = let pairDigit = head . drop s . take (succ s) $ r
                     in (x', pairDigit) : f xs' s (tail r)

matchingSum :: [(Int, Int)] -> Int
matchingSum = sum . map fst . filter (uncurry (==))

numberOne :: IO () 
numberOne = do
  input <- map (\x -> read [x] :: Int) . head . lines <$> readFile "/Users/nwest/AoC/2017/1"
  print . captcha $ input
  print . matchingSum . digitPairs $ input

-----------------------------------------

removeNegations :: String -> String
removeNegations = f ""
  where
    f acc (a:b:rest) | a == '!' = f acc rest
                     | otherwise = f (acc ++ [a]) $ b:rest
    f acc rest = acc ++ rest
{-
foldl' f ""
  where 
    f (b:bs) a | a == '!'  = bs 
               | otherwise = a:b:bs
               -}

removeGarbage :: String -> String
removeGarbage = f ""
  where
    f acc [] = acc
    f acc (y:ys) | y == '<'  = f acc . tail . dropWhile (/= '>') $ ys
                 | otherwise = f (acc ++ [y]) ys

removeGarbage' :: String -> String
removeGarbage' = fst . foldl' f ("", True)
  where
    f (xs, b) x | b, x == '<' = (xs, False)
                | b           = (xs ++ [x], True)
                | x == '>'    = (xs, True)
                | otherwise   = (xs, False)

countGarbage' :: String -> Int
countGarbage' = fst . foldl' f (0, True)
  where
    f (a, b) x | b, x == '<' = (a, False)
               | b           = (a, True)
               | x == '>'    = (a, True)
               | otherwise   = (succ a, False)

countDepthScore :: String -> Int
countDepthScore = snd . foldr f (0,0)
  where
    f x acc@(a, _) | x == '}'  = first succ acc
                   | x == '{'  = (pred *** (a +)) acc
                   | otherwise = acc

numberNine :: IO ()
numberNine = do 
  input <- head . lines <$> readFile "/Users/nwest/AoC/2017/9"
  let cleanInput = removeNegations input
      removedGarbage = removeGarbage' cleanInput
  print . countDepthScore $ removedGarbage
  print . countGarbage' $ cleanInput


-----------------------------------------

type Hash = Map Int Int

hashReverse :: Hash -> Int -> Int -> Hash
hashReverse m p i = let size   = M.size m
                        keys   = take i . map (`mod` size) $ [p..]
                        values = reverse . map (m M.!) $ keys
                        helper (k,v) = M.insert k v
                    in foldr helper m (zip keys values)

hash :: Hash -> Int -> [Int] -> Hash
hash m c xs = helper m c xs 0 0
  where
    helper :: Hash -> Int -> [Int] -> Int -> Int -> Hash
    helper m' c' [] p s  = helper m' (pred c') xs p s
    helper m' c' (a:as) p s | c' == 0 = m'
                            | otherwise = let m'' = hashReverse m' p a
                                              p'  = p + a + s
                                          in helper m'' c' as p' . succ $ s

baseHash :: Hash
baseHash = M.fromList . zip [0..255] $ [0..255]

solvePart1 :: Int
solvePart1 = product . take 2 . M.elems . hash baseHash 1 $ [227, 169, 3, 166, 246, 201, 0, 47, 1, 255, 2, 254, 96, 3, 97, 144]

sparseHash :: String -> [Int]
sparseHash s = let xs = map ord s ++ [17, 31, 73, 47, 23]
               in M.elems . hash baseHash 64 $ xs

denseHash :: String -> [Int]
denseHash = map (foldr1 xor) . by 16 . sparseHash

solvePart2 :: String
solvePart2 = concatMap (printf "%02x") . denseHash $ "227,169,3,166,246,201,0,47,1,255,2,254,96,3,97,144"

by :: Int -> [a] -> [[a]]
by _ [] = []
by n xs = take n xs : by n (drop n xs)

-------------------

type Cube = (Int, Int, Int)

move :: Cube -> String -> Cube
move (x, y, z) "n"  = (x, succ y, pred z)
move (x, y, z) "ne" = (succ x, y, pred z)
move (x, y, z) "se" = (succ x, pred y, z)
move (x, y, z) "s"  = (x, pred y, succ z)
move (x, y, z) "sw" = (pred x, y, succ z)
move (x, y, z) "nw" = (pred x, succ y, z)
move c _ = c

route :: [String] -> (Cube, Int)
route = foldl' f ((0, 0, 0), 0)
  where
    f (c, m) x = let c' = move c x
                     d = originDistance c'
                 in (c', max m d)

cleanDirections :: String -> [String]
cleanDirections = words . foldr f ""
  where
    f ',' = (' ' :)
    f x   = (x :)

originDistance :: Cube -> Int
originDistance (x, y, z) = (abs x + abs y + abs z) `div` 2

numberEleven :: IO ()
numberEleven = do
  input <- cleanDirections <$> readFile "/Users/nwest/AoC/2017/11"
  let (c, d) = route input
  print . originDistance $ c
  print d

-------------------
