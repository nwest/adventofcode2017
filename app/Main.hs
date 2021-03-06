{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.List (foldl', sort, elemIndex, group, sortOn)
import Control.Arrow

import qualified Data.Map as M hiding (Map)
import Data.Map (Map)

import qualified Data.Set as S hiding (Set)
import Data.Set (Set)

import Text.Printf
import Data.Char (ord)
import Data.Bits (xor)

import Data.Ord


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

combinations :: [a] -> [(a, a)]
combinations xs = concat [ zip (repeat b) . drop a $ xs | (a, b) <- zip [1..] xs ]

checksum :: [Int] -> Int
checksum xs = maximum xs - minimum xs

checksum2 :: [Int] -> Int
checksum2 xs = let combos = combinations xs ++ combinations (reverse xs)
                   (x, y) = head . filter (\(a, b) -> a `mod` b == 0) $ combos
               in x `div` y

numberTwo :: IO ()
numberTwo = do
  input <- map (map (\x -> read x :: Int) . words) . lines <$> readFile "/Users/nwest/AoC/2017/2"
  print . sum . map checksum $ input
  print . sum . map checksum2 $ input

-----------------------------------------

type Coordinate = (Int, Int)
type SquareSize = Int
data Movement = UpS | DownS | LeftS | RightS deriving (Show)

manhattan :: Coordinate -> Coordinate -> Int
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

adjacent :: Coordinate -> Coordinate -> Bool
adjacent c c' = manhattan c c' == 1

diagonal :: Coordinate -> Coordinate -> Bool
diagonal (x,y) (x',y') = abs (x-x') == 1 && abs (y-y') == 1

path :: SquareSize -> [Movement]
path s = let m = replicate (pred s)
         in concat [tail (m UpS), m LeftS, m DownS, m RightS, [RightS], path (s+2)]

moveC :: Coordinate -> Movement -> Coordinate
moveC (x,y) UpS    = (x, succ y)
moveC (x,y) DownS  = (x, pred y)
moveC (x,y) LeftS  = (pred x, y)
moveC (x,y) RightS = (succ x, y)

spiralTo :: Int -> Int
spiralTo end = let (x,y) = foldl' moveC (1,0) . take (end - 2) . path $ 3
               in abs x + abs y

firstBiggerThan :: Int -> Int
firstBiggerThan i = head . filter (> i) . map fst . foldl' f [(1, (0,0)), (1, (1,0))] . take 1000 . path $ 3
  where
    f xs m = let prev = last xs
                 nextCoord = moveC (snd prev) m
                 nextTotal = sum . map fst . filter (\(_, c)-> diagonal nextCoord c || adjacent nextCoord c) $ xs
             in xs ++ [(nextTotal, nextCoord)]

-----------------------------------------

validPassword :: [String] -> Bool
validPassword [] = True
validPassword (x:xs) = x `notElem` xs && validPassword xs

numberFour :: IO ()
numberFour = do
  input <- map words . lines <$> readFile "/Users/nwest/AoC/2017/4"
  let validPasswords = length . filter validPassword
  print . validPasswords $ input
  print . validPasswords . map (map sort) $ input

-----------------------------------------

type Jumps = Map Int Int
type Times = Int

trampoline :: Jumps -> Offset -> Times -> Times
trampoline jumps offset times = if M.notMember offset jumps
                                  then times
                                  else let oldValue = jumps M.! offset
                                           newJumps = M.adjust succ offset jumps
                                       in trampoline newJumps (offset + oldValue) (succ times)

trampoline2 :: Jumps -> Offset -> Times -> Times
trampoline2 jumps offset times = if M.notMember offset jumps
                                    then times
                                    else let oldValue = jumps M.! offset
                                             f = if oldValue > 2 then pred else succ
                                             newJumps = M.adjust f offset jumps
                                         in trampoline2 newJumps (offset + oldValue) (succ times)

numberFive :: IO ()
numberFive = do
  input <- map (\x -> read x :: Int) . lines <$> readFile "/Users/nwest/AoC/2017/5"
  let mapped = M.fromList . zip [0..] $ input
  print . trampoline mapped 0 $ 0
  print . trampoline2 mapped 0 $ 0

-----------------------------------------

type Memory = [Int]
type Offset = Int
type Allocation = Int

daySixInput :: [Int]
daySixInput = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]

distribute :: Memory -> Offset -> Allocation -> Memory
distribute m _ 0 = m
distribute m o a = let after = drop o m
                       blocksLeft = length after
                       moveDistance = min blocksLeft a
                       m' = take o m ++ (map succ . take moveDistance $ after) ++ drop moveDistance after
                   in distribute m' 0 (max 0 (a - blocksLeft))

maxMemOffset :: Memory -> Int
maxMemOffset m = let maxBlock = maximum m
                     offset = elemIndex maxBlock m
                 in f offset
  where
    f Nothing = 0
    f (Just x) = x

cycleMem :: Memory -> Memory
cycleMem m = let count = maximum m
                 offset = maxMemOffset m
                 m' = take offset m ++ [0] ++ drop (succ offset) m
             in distribute m' (succ offset) count


memoryCycles :: Memory -> Set Memory -> (Int, Memory)
memoryCycles m s = if S.member m s
                    then (S.size s, m)
                    else memoryCycles (cycleMem m) (S.insert m s)

numberSix :: Memory -> Int
numberSix m = fst (memoryCycles m S.empty)

numberSixB :: Memory -> Int
numberSixB m = let rep = snd (memoryCycles m S.empty)
               in numberSix rep

-----------------------------------------

type Groups = Map String [String]
type Weights = Map String Int
data Tower = Tower String Int [Tower] deriving Show

removeSpecial :: String -> String
removeSpecial = foldr (\x acc-> if x `notElem` "(),->" then x:acc else acc) ""

towerWeights :: [[String]] -> Map String Int
towerWeights = M.fromList . map (\xs -> (head xs, f xs))
  where
    f xs' = read ( xs' !! 1) :: Int

towerGroups :: [[String]] -> Map String [String]
towerGroups = M.fromList . map (\x-> (head x, drop 2 x)) . filter (\x-> length x > 2)

parseTower :: Groups -> Weights -> String -> Tower
parseTower g w s = Tower s (f (M.lookup s w)) (h (M.lookup s g))
  where
    f Nothing = 0
    f (Just x) = x
    h Nothing = []
    h (Just xs) = sortOn (Data.Ord.Down . towerWeight) . map (parseTower g w) $ xs 

towerWeight :: Tower -> Int
towerWeight (Tower _ i ts) = sum (i : map towerWeight ts)

subWeights :: Tower -> [Int]
subWeights (Tower _ _ ts) = map towerWeight ts

uniqueOffset :: [Int] -> Int
uniqueOffset [] = 0
uniqueOffset (x:xs) = if x `elem` xs then 1 + uniqueOffset xs else 0

hasUnique :: [Int] -> Bool
hasUnique xs = (length . group . sort $ xs) > 1

balanceTower :: Tower -> Tower
balanceTower t@(Tower _ _ ts) = if hasUnique (subWeights t) 
                                then let uniqueO = uniqueOffset (subWeights t)
                                     in balanceTower (head . drop uniqueO $ ts)
                                else t

parentID :: Groups -> String -> String
parentID g s = fst . head . filter (\(_,v)-> s `elem` v). M.toList $ g

parentTower :: Groups -> Weights -> Tower -> Tower
parentTower g w (Tower s _ _) = let parent = parentID g s
                                in parseTower g w parent

baseWeight :: Tower -> Int
baseWeight (Tower _ i _) = i

change :: Tower -> Int
change (Tower _ _ []) = 0
change (Tower _ _ ts) = let (x:xs) = map towerWeight ts
                        in head xs - x

numberSeven :: IO ()
numberSeven = do
  input <- map (words . removeSpecial) . lines <$> readFile "/Users/nwest/AoC/2017/7"
  let baseTower = head . head . filter (\x -> length x == 1) . group . sort . concatMap (\x -> head x : drop 2 x) $ input
      weights = towerWeights input
      groups = towerGroups input
      tower = parseTower groups weights baseTower
      unbalanced =  balanceTower tower
      unbalancedParent = parentTower groups weights unbalanced
  print baseTower
  print (baseWeight unbalanced + change unbalancedParent)

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
