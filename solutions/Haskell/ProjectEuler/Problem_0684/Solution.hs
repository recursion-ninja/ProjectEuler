{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedSums #-}
{-# LANGUAGE DataKinds #-}
--{-# Language KindSignatures, ScopedTypeVariables #-}

module ProjectEuler.Problem_0684.Solution
  ( -- * Data types
    -- ** Inverse Digit Sum
    inverseDigitSum
  , inverseDigitAggregation
    -- * Solver
--  , solveBinomialCoefficientFactorizationSum
  , solution
  ) where


import Data.Modular
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Sum(..))
--import Numeric.Natural
import Data.Proxy
import GHC.TypeNats hiding (Mod)
import GHC.TypeLits.Compare
import Data.Type.Equality ((:~:)( Refl ))
import Data.Word

type InverseDigitSumMod n = Sum (Word64 `Mod` n)

{- |
'inverseDigitSum' \( n \) computes the smallest number that has a digit sum of \( n \).

==== __Examples__

>>> inverseDigitSum 10
19
-} 
inverseDigitSum :: forall n . Modulus n => Word64 -> InverseDigitSumMod n
inverseDigitSum val =
    let (nines, prefix) = val `divMod` 9
        magnitude = 10 ^ nines
        addend1 = prefix * magnitude 
        addend2 = magnitude - 1
    in  Sum . toMod @n $ addend1 + addend2


{- |
An 'inverseDigitAggregation' \( k \) is defined as:
\( S\left(\, k\, \right) = \sum_{n=1}^{k} \mathtt{inverseDigitSum}\left(\, n \,\right) \)
==== __Examples__

>>> inverseDigitAggregation 20
1074
-} 
inverseDigitAggregation :: forall n . Modulus n => Word64 -> InverseDigitSumMod n
inverseDigitAggregation = foldMap (inverseDigitSum @n) . enumFromTo 1


fibonacciSequence :: NonEmpty Word64
fibonacciSequence =
    let gen ( a, b ) = ( b, a + b )
    in  fst <$> NE.iterate gen ( 0, 1 )


{- |

The Fibonacci sequence \(F_0, F_1, F_2, \ldots\) is defined recursively by \(F_{0} =0, F_{1} =1 \) and \(F_{n} =F_{n-1}+F_{n-2}\).
>
> Prove that
\[
\sum_{i=0}^{n} F_{i}=F_{n+2}-1 \qquad \text{for all } n \geq 0
\]

Use \(F_{n+1}+F_{n+2}=F_{n+3}\), to get:
 
\[
\sum_{i=0}^{n+1} F_{i}=\sum_{i=0}^{n} F_{i}+F_{n+1}=F_{n+2}-1+F_{n+1}=F_{n+1}+F_{n+2}-1=F_{n+3}-1
\]

Solution sums from \( i = 2 \) to \( \mathtt{limit} \), hence:

\[
\mathtt{solution}\left(\mathtt{limit}\right) = F_{\mathtt{limit} + 2} - 2 
\]
-}
solution :: Word -> Word64 -> Word64
solution limit modulus =
    let fibNumbers = take (fromEnum limit - 2) $ NE.drop 2 fibonacciSequence
        aggregation :: forall n . SNat n -> Word64
        aggregation SNat = case isLE (Proxy :: Proxy 1) (Proxy :: Proxy n) of
          Nothing -> 0
          Just Refl ->          
            let aggregator :: Word64 -> InverseDigitSumMod n
                aggregator = inverseDigitAggregation @n
            in  unMod . getSum $ foldMap aggregator fibNumbers
        
    in  withSomeSNat (fromIntegral modulus) aggregation
