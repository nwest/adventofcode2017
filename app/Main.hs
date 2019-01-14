{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.List (foldl')
import Control.Arrow


main :: IO ()
main = numberNine

byTwo :: [a] -> [[a]]
byTwo = reverse . foldl f []
  where
    f [] x = [[x]]
    f g@(a:as) x = if length a == 1 then (x:a):as else [x] : g

captcha :: [Int] -> Int
captcha s = let fixedInput = reverse $ (head s : reverse s)
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
    f (xs, b) x | b == True, x == '<' = (xs, False)
                | b == True           = (xs ++ [x], True)
                | x == '>'            = (xs, True)
                | otherwise           = (xs, False)

countGarbage' :: String -> Int
countGarbage' = fst . foldl' f (0, True)
  where
    f (a, b) x | b == True, x == '<' = (a, False)
               | b == True           = (a, True)
               | x == '>'            = (a, True)
               | otherwise           = (succ a, False)

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

tenTestKnot :: [Int]
tenTestKnot = [0..4]

tenTestInput :: [Int]
tenTestInput = [3, 4, 1, 5]

tenKnot :: [Int]
tenKnot = [0..255]

tenInput :: [Int]
tenInput = [227, 169, 3, 166, 246, 201, 0, 47, 1, 255, 2, 254, 96, 3, 97, 144]

numberTen :: [Int] -> [Int] -> (Int, [Int])
numberTen knot input = let newKnot = fst . foldl' f (knot, 0) $ input
                           rotation = sum input + sum [0..(length input - 1)]
                           --offset  = length input - (rotation `mod` length input)
                       in (rotation, newKnot) -- take 2 . drop offset . cycle $ newKnot
  where
    f (knot', skip) len = let reversed = reverse . take len $ knot'
                              rotate   = (++ reversed) . drop len $ knot'
                              skipped  = take skip rotate
                              rotate'  = (++ skipped) . drop skip $ rotate
                          in (rotate', succ skip)
