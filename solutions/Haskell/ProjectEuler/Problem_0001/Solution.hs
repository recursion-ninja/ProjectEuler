module ProjectEuler.Problem_0001.Solution
 ( solution
 ) where

import Control.Arrow       ((***))
import Data.List           (foldl1',partition)
import Data.Set            (elems,fromList)

{--
 - Use Inclusion-Exclusion principle
 - Along with closed form calculation
 - To calculate to arbitrarily large bound
 - with arbirtarily many divisors
 - in `O(2^n)` where `n` is the number of divisors
 --}

-- | Generalized solution to the Project Euler #0001 problem
solution :: Integral a => a -> [a] -> a
solution limit divisors
  |  (not . positive) limit
  || null divisors' = 0
  |  otherwise = uncurry (-) $ minuendSubtrahend (limit-1) (reduce divisors')
  where
    divisors' = filter positive divisors
    positive  = (>0)

-- | Remove duplicates and multiples from integral list
reduce :: (Integral a) => [a] -> [a]
reduce xs = filter noneDivides $ uniqueElems xs
  where
    uniqueElems      = elems . fromList -- use Set to force uniqueness
    noneDivides x    = not $ (x `multipleOf`) `any` xs
    n `multipleOf` m = m < n && n `mod` m == 0 

-- | The minuend and subtrahend
-- | derived from the inclusion/exclusion principle
-- | which evaluates to the sum of multiples
minuendSubtrahend :: (Integral a) => a -> [a] -> (a,a)
minuendSubtrahend n = both total . inclusionExclusion
  where
    total     = sum . map summation
    summation = naturalSumModulo n . leastCommonMultiple

-- | Least common multiple of an integral list
leastCommonMultiple :: Integral a => [a] -> a
leastCommonMultiple = foldl1' lcm

-- | Map the function across a homogenous arrow
both :: (a->b) -> (a,a) -> (b,b)
both f = f *** f

-- | Sum of the multiples of `factor` less then equal to `limit`
naturalSumModulo :: (Integral a) => a -> a -> a
naturalSumModulo limit factor = factor * n*(n+1) `div` 2
  where n = limit `div`  factor

-- | Inclusive and exclusive partition of the powerset
inclusionExclusion :: [b] -> ([[b]],[[b]])
inclusionExclusion
  = both (fmap snd)
  . partition (odd . fst)
  . nonEmptySubsequences

-- | Set of subsets along with the subset length
nonEmptySubsequences         :: [a] -> [(Int,[a])]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  (1,[x]) : foldr f [] (nonEmptySubsequences xs)
  where f (n,ys) zs = (n,ys) : (n+1,x:ys) : zs
