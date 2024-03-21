{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedSums #-}

module ProjectEuler.Problem_0684.Solution
  ( -- * Data types
    -- ** Inverse Digit Sum
    inverseDigitSum
    -- * Solver
--  , solveBinomialCoefficientFactorizationSum
  , solution
  ) where


import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Sum(..))
import Math.NumberTheory.Primes (Prime, unPrime)
import Math.NumberTheory.Recurrences.Linear (fibonacciPair)
import Numeric.Natural


{- |
'inverseDigitSum' \( n \) computes the smallest number that has a digit sum of \( n \).

==== __Examples__

>>> inverseDigitSum 10
19
-} 
inverseDigitSum :: Natural -> Natural
inverseDigitSum = undefined


{- |
An 'inverseDigitAggregation' \( k \) is defined as:
\( S\left(\, k\, \right) = \sum_{n=1}^{k} \mathtt{inverseDigitSum}\left(\, n \,\right) \)
==== __Examples__

>>> inverseDigitAggregation 20
1074
-} 
inverseDigitAggregation :: Natural -> Natural
inverseDigitAggregation = undefined


fibonacciSequence :: NonEmpty Natural
fibonacciSequence =
    let gen ( a, b ) = ( b, a + b )
    in  fst <$> NE.iterate gen ( 0, 1 )


solution :: Word -> Natural -> Natural
solution limit modulus =
    let fibNumbers = take (fromEnum limit - 2) $ NE.drop 2 fibonacciSequence
        aggregator = Sum . (`mod` modulus) . inverseDigitAggregation
    in  getSum $ foldMap aggregator fibNumbers
