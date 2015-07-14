module ProjectEuler.Problem_0003.Solution
  ( largestPrimeFactor
  , solution
  ) where

import Math.NumberTheory.Primes.Factorisation (factorise')

solution :: Integer -> Integer
solution = largestPrimeFactor

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor = maximum . fmap fst . factorise'
