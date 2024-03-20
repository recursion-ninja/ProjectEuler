{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedSums #-}

module ProjectEuler.Problem_0808.Solution
  ( -- * Data types
    -- ** Reversible Prime Square
    ReversiblePrimeSquare()
  , takeReversiblePrimeSquare
  , makeReversiblePrimeSquare
  , makeReversiblePrimeSquarePair
    -- ** Prime Set
  , PrimeSet() 
  , deletePrime
  , insertPrime
  , queryPrime
    -- * Solver
--  , solveConcealedSquare
  , solution
  ) where


import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import Data.IntSet qualified as ISet
import Data.Maybe (mapMaybe)
import Data.Semigroup (Sum(..))
import Math.NumberTheory.Primes (Prime, UniqueFactorisation, isPrime, unPrime, nextPrime)
import Math.NumberTheory.Roots (exactSquareRoot)
import Control.Monad ((<=<))
import Numeric.Natural


{--
import Debug.Trace
tr :: Show a => String -> a -> a
tr str val = trace (str <> ":\t" <> show val) val
--}
{--}
tr :: b -> a -> a
tr = const id
{--}


{- |
We call a number a reversible prime square (RSP) if:

  1. It is not a palindrome, and
  2. It is the square of a prime, and
  3. Its reverse is also the square of a prime.

/Note:/ Using number theory, every RSP number \( n \) will have a "sister" RSP number \( q \) such that:

\( reverse\left(\,n\,\right) = q \)
-}
newtype ReversiblePrimeSquare = RPS (Prime Natural)


deriving stock instance Show ReversiblePrimeSquare


newtype PrimeSet = PrimeSet IntSet


deriving newtype instance Eq PrimeSet


deriving newtype instance Ord PrimeSet


deriving newtype instance Monoid PrimeSet


deriving newtype instance Semigroup PrimeSet


deriving newtype instance Show PrimeSet



insertPrime :: Enum (Prime n) => Prime n -> PrimeSet -> PrimeSet
insertPrime p (PrimeSet ps) =
    let !primeIndex = fromEnum p
        result = PrimeSet $ ISet.insert primeIndex ps
    in  result `seq` result


deletePrime :: Enum (Prime n) => Prime n -> PrimeSet -> PrimeSet
deletePrime p (PrimeSet ps) =
    let !primeIndex = fromEnum p
        result = PrimeSet $ ISet.delete primeIndex ps
    in  result `seq` result


queryPrime :: Enum (Prime n) => Prime n -> PrimeSet -> Bool
queryPrime p (PrimeSet ps) =
    let primeIndex = fromEnum p
    in  ISet.member primeIndex ps



takeReversiblePrimeSquare :: ReversiblePrimeSquare -> Natural
takeReversiblePrimeSquare (RPS p) =
    let rps = unPrime p
    in  rps * rps

 
makeReversiblePrimeSquare :: Prime Natural -> Maybe ReversiblePrimeSquare
makeReversiblePrimeSquare = fmap fst . makeReversiblePrimeSquarePair


makeReversiblePrimeSquarePair :: Prime Natural -> Maybe (ReversiblePrimeSquare, ReversiblePrimeSquare)
makeReversiblePrimeSquarePair p =
    let squared x = x * x 
        wrap q = (RPS p, RPS q)
        validation = fmap wrap . (isSquarePrime <=< isNotPalindrome10) . squared
    in  validation $ unPrime p


isSquarePrime :: (Integral n, UniqueFactorisation n) => n -> Maybe (Prime n)
isSquarePrime = isPrime <=< exactSquareRoot


isNotPalindrome10 :: (Integral n, Read n) => n -> Maybe n
isNotPalindrome10 num =
    let str = show . abs $ toInteger num
        rev = reverse str
    in  if str == rev
        then Nothing
        else Just $ read rev



solution :: Word -> Natural
solution firstN =
    let firstRSPs = take (fromEnum firstN) $ mapMaybe makeReversiblePrimeSquare [ nextPrime 2 .. ]
        aggregate = getSum . foldMap (Sum . takeReversiblePrimeSquare)
    in  aggregate firstRSPs
