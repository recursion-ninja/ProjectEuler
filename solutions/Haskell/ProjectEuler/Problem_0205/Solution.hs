{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language StrictData #-}

module ProjectEuler.Problem_0205.Solution
  ( -- * Die data-types
    Die(..)
  , DieFace(..)
  , DieGroup(..)
    -- * Die functions
  , dieFaces
  , dieFromFaceValues
  , hasFaceValue
  , maxFace
  , numFacesOf
    -- * Die combinatorics
  , rollsGreaterThan
  , compareRollDistribution
  , rollOutcomeTable
    -- * Other
  , renderRational
  ) where

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Data.Array.IArray
import Data.Foldable (fold, foldl',toList)
import Data.IntMultiSet (IntMultiSet, findMax, findMin, member, occur)
import Data.IntMultiSet qualified as IMS (fromList, toAscList)
import Data.IntMap.Strict (IntMap, toAscList)
import Data.List (intersperse)
import Data.Map.Strict ((!?), Map, singleton, unionsWith)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%), denominator, numerator)
import Data.Sequence (Seq(..), (|>), reverse, splitAt, tails) -- hiding (intersperse, length)
import Data.Word (Word8, Word64)
import GHC.Exts (fromList)
--import           Data.List
--import Data.Matrix
--import           Data.MonoTraversable
--import           Data.Vector      (Vector)
--import qualified Data.Vector as V
import Prelude hiding (reverse, splitAt)

import Debug.Trace



newtype Die = Die { faceMultiSet :: IntMultiSet }
    deriving (Eq, Ord, Show)


newtype DieFace = DieFace Int
    deriving (Bounded, Enum, Eq, Ix, Ord, Show)


dieFromFaceValues :: (Enum v, Foldable f) => f v -> Die
dieFromFaceValues = Die . IMS.fromList . fmap fromEnum . toList


dieFaces :: Enum v => Die -> [v]
dieFaces = fmap toEnum . IMS.toAscList . faceMultiSet


maxFace :: Enum v => Die -> v
maxFace = toEnum . findMax . faceMultiSet


minFace :: Enum v => Die -> v
minFace = toEnum . findMin . faceMultiSet


numFacesOf :: Enum v => Die -> v -> Word
numFacesOf die = toEnum . (`occur` faceMultiSet die) . fromEnum


hasFaceValue :: Enum v => Die -> v -> Bool
hasFaceValue die = (`member` faceMultiSet die) . fromEnum


data  DieGroup
    = DieGroup {-# UNPACK #-} Word8 {-# UNPACK #-} Die
    deriving (Eq, Ord, Show)


dieCount :: Enum c => DieGroup -> c
dieCount (DieGroup count _) = toEnum $ fromEnum count


dieShape :: DieGroup -> Die
dieShape (DieGroup _ die) = die


maxRollValue :: Enum v => DieGroup -> v
maxRollValue = getRollValue maxFace


minRollValue :: Enum v => DieGroup -> v
minRollValue = getRollValue minFace


getRollValue :: Enum v => (Die -> Int) -> DieGroup -> v
getRollValue f dice =
    let count = dieCount dice
        die   = dieShape dice
    in  toEnum $ count * f die


{- |
The number of way to add M numbers, ranged 1 to K, to a give a desired sum N.
Use polynomial to achieve the goal.
For example, suppose there are M numbers, each one ranged from 1 to K inclusive, if we consider the polynomial:

$$ (𝑥^1 + 𝑥^2 + x^3 + ⋯ + x^K)^M $$

the way to add those M numbers to get N is the coefficient of the term x^N!

# Stars and bars (combinatorics)

## Theorem one

For any pair of positive integers N and K, the number of K-tuples of positive integers whose sum is N;
is equal to the number of (K − 1)-element subsets of a set with (N − 1) elements.

For example, if N = 10 and K = 4,
the theorem gives the number of solutions to x_1 + x_2 + x_3 + x_4 = 10 (with x_1, x_2, x_3, x_4 > 0) as the binomial coefficient (N - 1) `choose` (K - 1).


    ( n − 1 k − 1 ) = ( 10 − 1 4 − 1 ) = ( 9 3 ) = 84. {\displaystyle {\binom {n-1}{k-1}}={\binom {10-1}{4-1}}={\binom {9}{3}}=84.}

This corresponds to compositions of an integer.

The number k-composition

10 = 7 + 1 + 1 + 1

10 = 6 + 2 + 1 + 1
10 = 6 + 1 + 2 + 1
10 = 6 + 1 + 1 + 2

10 = 5 + 3 + 1 + 1
10 = 5 + 1 + 3 + 1
10 = 5 + 1 + 1 + 3
10 = 5 + 2 + 2 + 1
10 = 5 + 2 + 1 + 2
10 = 5 + 1 + 2 + 2

10 = 4 + 4 + 1 + 1
10 = 4 + 1 + 4 + 1
10 = 4 + 1 + 1 + 4
10 = 4 + 3 + 2 + 1
10 = 4 + 3 + 1 + 2
10 = 4 + 2 + 3 + 1
10 = 4 + 2 + 1 + 3
10 = 4 + 1 + 3 + 2
10 = 4 + 1 + 2 + 3



10 = 3 + 3 + 2 + 2

---

    dp(m, n, k) = sum i in [1, m] $ \i -> i * dp( m, n - i, k - 1)


---

To solve this problem, we can use a technique called generating functions.
Let's define the generating function G(x) as follows:

$$G(x) = (x + x^2 + x^3 + x^4)^9$$

Each term in the parentheses represents one of the four possible numbers we can choose from the set {1, 2, 3, 4}. The exponent of each term represents the number of times we choose that number.
For example, the term x^2 represents choosing the number 2 twice.

Expanding the generating function G(x) will give us all possible ways of choosing 9 numbers from the set {1, 2, 3, 4} and adding them up. We can then find the coefficient of the term x^23 to determine the number of ways to get a sum of 23.

To find this coefficient, we can use a computer algebra system like WolframAlpha or a tool like the Binomial Coefficient Calculator. The coefficient of x^23 in G(x) is 1078, so there are 1078 unique ways to choose 9 numbers from the set {1, 2, 3, 4} that add up to 23.

Alternatively, we could use a more manual method called dynamic programming to solve the problem.
We can create a 2D array dp where dp[i][j] represents the number of ways to use the first i numbers from the set {1, 2, 3, 4} to get a sum of j.
We can initialize dp[0][0] = 1, since there is one way to get a sum of 0 using zero numbers. Then, we can use the following recurrence relation to fill in the rest of the array:


The first term dp[i-1][j] represents the case where we don't use the ith number at all. The remaining terms represent the cases where we use the ith number once, twice, or three times.

Finally, the answer to the problem is given by dp[4][23], since we want to use all four numbers from the set {1, 2, 3, 4}. Using this method, we also get 1078 as the answer.


    dp( m, n, k ) =
        let b = min { m, max { 0, n - k } }
        in  sum i in [1, b] $ \i -> i * dp( m, n - i, k - 1)

-}


newtype M = M Word8 deriving (Bounded, Enum, Eq, Integral, Ix, Num, Ord, Real, Show)


newtype N = N Int   deriving (Bounded, Enum, Eq, Integral, Ix, Num, Ord, Real, Show)


newtype K = K Word8 deriving (Bounded, Enum, Eq, Integral, Ix, Num, Ord, Real, Show)


rollsGreaterThan :: DieGroup -> DieGroup -> Rational
rollsGreaterThan dice1 dice2 =
    let outcomeDistribution = compareRollDistribution dice1 dice2
        universe = sum $ fromIntegral <$> outcomeDistribution
        leftWins = fromIntegral . fromMaybe 0 $ outcomeDistribution !? GT
    in  leftWins % universe


compareRollDistribution :: DieGroup -> DieGroup -> Map Ordering Word64
compareRollDistribution dice1 dice2 =
    let dist1 = rollDistribution dice1 :: IntMap Word64
        dist2 = rollDistribution dice2 :: IntMap Word64
        cmp (val1, freq1) (val2, freq2) = singleton (val1 `compare` val2) $ freq1 * freq2
    in  unionsWith (+) $ cmp <$> toAscList dist1 <*> (toAscList dist2 :: [(Int, Word64)])


rollDistribution :: DieGroup -> IntMap Word64
rollDistribution dice =
  let table :: Array (N, K) Word64
      table = rollOutcomeTable dice

      k :: K
      k = dieCount dice

      upperBound :: N
      upperBound = maxRollValue dice

      lowerBound :: N
      lowerBound = minRollValue dice

      frequency :: N -> (Int, Word64)
      frequency n = (fromEnum n, table ! (n, k))

  in  fromList $ frequency <$> [ lowerBound .. upperBound ]


rollOutcomeTable :: DieGroup -> Array (N, K) Word64
rollOutcomeTable diceRoll | trace (show diceRoll) False = undefined
rollOutcomeTable dice@(DieGroup count die) =
    let
        enum :: (Enum a, Enum b) => a -> b
        enum = toEnum . fromEnum

        maxRollVal :: N
        maxRollVal = max 0 $ maxRollValue dice

        minRollVal :: N
        minRollVal = min 0 $ minRollValue dice

        maxSummand :: K
        maxSummand = enum count

        minSummand :: K
        minSummand = K 1

        upperBound :: (N, K)
        upperBound = (maxRollVal, maxSummand)

        lowerBound :: (N, K)
        lowerBound = (minRollVal, minSummand)

        arrayBound :: ((N, K), (N, K))
        arrayBound = tr "arrayBound" (lowerBound, upperBound)

        -- Subtraction which clamps to 0
{-
        sub :: Word8 -> Word8 -> Word8
        sub x y
            | x <= y = 0
            | otherwise = x - y
-}

        m :: Word8
        m = maxFace die

        tr str x = trace (str <> ": " <> show x) x

        arrayElements = tr "arrayElems" $ range arrayBound
{-
    dp( m, n, k ) =
        let b = min { m, max { 0, n - k } }
        in  sum i in [1, b] $ \i -> i * dp( m, n - i, k - 1)

-}
        memo :: Array (N, K) Word64
        memo = (trace "memo" array) arrayBound $
            let gen :: (N, K) -> Word64
                gen p | trace ("gen @ " <> show p) False = undefined
                gen p@(N n, K k) =
                    if k <= 1
                    then if die `hasFaceValue` n
                         then 1
                         else 0
                    else
                        let f  :: Int -> Word64
                            f i = ref (n - i) (k - 1)

                            logGen =
                                let named str x = fold [ "  ", str, " = ", show x ]
                                in  unlines
                                      [ "gen @ " <> show p
                                      , named "m" m
                                      , named "n" n
                                      , named "k" k
                                      ]

                        in  sum $ f <$> trace logGen (dieFaces die)

                ref :: Int -> Word8 -> Word64
                ref n k = (\x -> trace ("ref @ " <> show (n,k) <> " = " <> show x) x) $
                    if N n < minRollVal
                    then 0
                    else if k <= 1
                         then if die `hasFaceValue` n
                              then 1
                              else 0
                         else memo ! (tr "memo @ " (N n, K k) )

            in  (id &&& gen) <$> arrayElements
    in  memo


{-
solution :: Int
solution = length $ filter allSquaresDisplayed allAssignments
  where
    allAssignments = (\[x,y] -> (x,y)) <$> combinations 2 dieAssignments
-}


renderRational :: Rational -> String
renderRational rat =
    let num = numerator rat
        den = denominator rat
    in  case num `quotRem` den of
            (q,0) -> show q
            (q,r) ->
                let -- Unicode literal for the "combining overline character".
                    -- Place an overline above the character /before/ itself in the string.
                    addOverline =
                        let !c = '\x0305'
                        in  (c:) . intersperse c

                    digitMax = length . show $ max num den

                    render = foldMap (show . fst)

                    go !v xs =
                      case hasCycle xs of
                        Just (static, repeating) -> render static <> addOverline (render repeating)
                        _ | v == 0 || length xs > digitMax -> render xs
                        _  -> let t@(_,m) = (v*10) `quotRem` den in go m $ xs |> t

                    hasCycle x =
                        let dropEmptySeq :: Seq a -> Seq a
                            dropEmptySeq  xs@Empty  = xs
                            dropEmptySeq (xs :|> _) = xs

                            !n = length x

                            f a e = a <|> let !m = length e
                                              !(s,t) = splitAt (n - (2 * m)) x
                                          in  if t == e <> e
                                              then Just (s, e)
                                              else Nothing
                        in  foldl' f Nothing . reverse . dropEmptySeq $ tails x

                in  fold [ show q, ".", toList (go (if q == 0 then num else r) mempty)]
