module ProjectEuler.Problem_0001.Test
 ( main
 ) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***))
import Data.Set            (elems, fromList)
import Test.QuickCheck
import qualified ProjectEuler.Problem_0001.Solution as P0001

main ::  IO ()
main = do
  quickCheck equivelence_naive_algorithm

equivelence_naive_algorithm :: (Int,[Int]) -> Bool
equivelence_naive_algorithm (limit,divisors) =
    limit > 1000 -- High limits cause runtime issues from naive solution
 || P0001.solution limit divisors == naiveSolution limit divisors

n `divides` x = x `mod` n == 0

naiveSolution limit divisors
 | limit < 1 = 0 
 | otherwise = sum [ x | x <- [1..limit-1], any (`divides` x) divisors' ]
 where divisors' = filter (>0) divisors
