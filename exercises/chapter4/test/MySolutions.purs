module Test.MySolutions where

import Data.Path
import Prelude
import Test.Examples
import Control.Alternative (guard)
import Data.Array (concat, cons, filter, find, foldl, head, length, tail, (..), (:))
import Data.Maybe (fromMaybe, Maybe)

isEven :: Int -> Boolean
isEven n = case n `mod` 2 of
  0 -> true
  _ -> false

countEven :: Array Int -> Int
countEven [] = 0

countEven xs = count + (countEven (fromMaybe [] $ tail xs))
  where
  count = if isEven (fromMaybe 1 $ head xs) then 1 else 0

squared :: Array Number -> Array Number
squared xs = (\n -> n * n) <$> xs

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\n -> n >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (\n -> n >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime x = length (factors x) == 2
  where
  factors n = do
    i <- 1 .. n
    j <- 1 .. n
    guard $ i * j == n
    pure [ i, j ]

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct x y = do
  i <- x
  j <- y
  pure [ i, j ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []

  factorize divisor dividend =
    if dividend `mod` divisor == 0 then
      cons divisor $ factorize (divisor) (dividend / divisor)
    else
      factorize (divisor + 1) dividend

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc el -> cons el acc) []

allTrue :: Array Boolean -> Boolean
allTrue xs = length xs == folded
  where
  folded = foldl (\acc el -> if el then acc + 1 else acc) 0 xs

fibTailRec :: Int -> Int
fibTailRec 0 = 0

fibTailRec 1 = 1

fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

onlyFiles :: Path -> Array Path
onlyFiles file = filter (not isDirectory) $ allFiles file

whereIs :: Path -> String -> Maybe Path
whereIs path fileName =
  head
    $ do
        path' <- allFiles path
        child <- ls path'
        guard $ filename child == filename path' <> fileName
        pure path'

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path)
  where
  loop :: Array Path -> Path -> Array Path
  loop [ largest, smallest ] current
    | size current < size smallest = [ largest, current ]
    | size current > size largest = [ current, smallest ]
    | otherwise = [ largest, smallest ]

  loop [ last ] current
    | size current < size last = [ current, last ]
    | otherwise = [ last, current ]

  loop arr current = current : arr
