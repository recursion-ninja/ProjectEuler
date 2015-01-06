module ProjectEuler.Problem_0001.Solution
 ( solution
 ) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***))
import Data.Set            (elems, fromList)

{--
 - Use Inclusion-Exclusion principle
 - Along with closed form calculation
 - To calculate to arbitrarily large bound
 - with arbirtarily many divisors
 --}

solution :: Integral a => a -> [a] -> a
solution limit divisors =
  uncurry (-) $ getMinuendSubtrahend (limit-1) (reduce . elems $ fromList divisors)

getMinuendSubtrahend :: (Integral a) => a -> [a] -> (a,a)
getMinuendSubtrahend l =
   both (sum . map (naturalSumModulo l . product)) . inclusionExclusion

inclusionExclusion :: [b] -> ([[b]],[[b]])
inclusionExclusion xs =
    both (concatMap (choose xs))
    . (
    (,)
    <$>  filter odd 
    <*>  filter even
    ) $  (enumFromTo 1 . length) xs
    where 
      choose :: [b] -> Int -> [[b]]
      _      `choose` 0 = [[]]
      []     `choose` _ =  []
      (y:ys) `choose` k =  (y:) `fmap` (ys `choose` (k-1)) ++ ys `choose` k

both :: (a->b) -> (a,a) -> (b,b)
both f = f *** f

reduce :: (Integral a) => [a] -> [a]
reduce []     = []
reduce (x:xs) = (x:) . reduce $ filter ((/=0).mod x) xs

naturalSumModulo :: (Integral a) => a -> a -> a
naturalSumModulo l x = x*n*(n+1) `div` 2
  where n = l `div` x

