module ProjectEuler.Problem_0002.Solution
 ( solution
 ) where

{-
 - Notes:
 - Rely on lazy evaluation
 - and generate fibonacci numbers tail recursively
 -}

solution :: Integer -> Integer
solution limit = sum . filter even $ takeWhile (<=limit) fibonacciSequence

fibonacciSequence :: [Integer]
fibonacciSequence = 
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)


