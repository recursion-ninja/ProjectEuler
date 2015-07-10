module ProjectEuler.Problem_0001.Answer
 ( answer
 ) where

import qualified ProjectEuler.Problem_0001.Solution as P0001 (solution)

defaultLimit    = 1000
defaultDivisors = [3,5]

answer = P0001.solution
           defaultLimit
           defaultDivisors
