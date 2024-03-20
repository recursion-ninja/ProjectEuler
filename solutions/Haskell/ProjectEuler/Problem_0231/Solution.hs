{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedSums #-}

module ProjectEuler.Problem_0231.Solution
  ( -- * Data types
    -- ** Reversible Prime Square
    BinomialCoefficient()
  , choose
  , expand
    -- * Solver
--  , solveBinomialCoefficientFactorizationSum
  , solution
  ) where


import Data.Semigroup (Sum(..))
import Math.NumberTheory.Primes (Prime, unPrime)
import Math.NumberTheory.Recurrences.Bilinear (binomialFactors)
import Numeric.Natural


data BinomialCoefficient = BinomC Word Word


instance Show BinomialCoefficient where

    show (BinomC n k) = unwords [ show n, "choose", show k ]


deriving stock instance Eq BinomialCoefficient


deriving stock instance Ord BinomialCoefficient


choose :: (Integral n, Integral k) => n -> k -> BinomialCoefficient
choose n k =
    let n' = abs $ toInteger n
        k' = abs $ toInteger k
        n'' = fromInteger n'
        k'' | k' >= n' = n''
            | otherwise = fromInteger  k'
    in  BinomC n'' k''


expand :: BinomialCoefficient -> [(Prime Word, Word)]
expand (BinomC n k) = binomialFactors n k


summationOfFactorization :: [(Prime Word, Word)] -> Natural
summationOfFactorization =
    let aggregator :: (Prime Word, Word) ->  Sum Natural
        aggregator (p, e) = Sum $ fromIntegral (unPrime p) * fromIntegral e 
    in  getSum . foldMap aggregator


solution :: BinomialCoefficient -> Natural
solution = summationOfFactorization . expand
