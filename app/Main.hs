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

byTwo :: [a] -> [[a]]
byTwo = reverse . foldl f []
  where
    f [] x = [[x]]
    f g@(a:as) x = if length a == 1 then (x:a):as else [x] : g

captcha :: [Int] -> Int
captcha s = let fixedInput = reverse (head s : reverse s)
                pairs = concatMap byTwo [fixedInput, tail fixedInput]
            in
              sum . map matchingCount $ pairs
  where 
    matchingCount [a, b] = if a == b then a else 0
    matchingCount _ = 0

numberOne :: IO () 
numberOne = do
  input <- map (\x -> read [x] :: Int) . head . lines <$> readFile "/Users/nwest/AoC/2017/1"
  print . captcha $ input

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

solvePart1 :: Int
solvePart1 = let m = M.fromList . zip [0..255] $ [0..255]
                 xs = [227, 169, 3, 166, 246, 201, 0, 47, 1, 255, 2, 254, 96, 3, 97, 144]
             in product . take 2 . M.elems . hash m 1 $ xs

hash64 :: Hash -> [Int] -> Hash
hash64 m = hash m 64

input10b :: String
input10b = "1,2,3"

inputPadding :: [Int]
inputPadding = [17, 31, 73, 47, 23]

ascii :: String -> [Int]
ascii = map ord

sparseHash :: String -> [Int]
sparseHash s = let m = M.fromList . zip [0..255] $ [0..255]
                   xs = ascii s ++ inputPadding
               in M.elems . hash64 m $ xs

densify :: [Int] -> Int
densify = foldr1 xor

denseHash :: String -> [Int]
denseHash = map densify . by16 . sparseHash

solvePart2 :: String -> String
solvePart2 = hex . denseHash

hex :: [Int] -> String
hex = concatMap (printf "%02x")

by16 :: [a] -> [[a]]
by16 = foldr f []
  where
    f x [] = [[x]]
    f x g@(a:as) = if length a < 16 then (x:a):as else [x] : g

