module ProjectEuler.Problem_0086.Solution
  ( solution
  ) where

import Math.NumberTheory.Powers.Squares (isSquare')

{- |
 Get the minimnal value M, such that the number of cuboids with dimensions less
 than or equal to M x M x M that have integral shortest paths from one corner to
 the opposite corner exceeds the supplied number.
-}
solution :: Int -> Int
solution n = pred . length $ takeWhile (<=n) cumulativeCuboids

{- |
 Each element of the list olds the /cumulative/ number of cuboids with integral
 shortest paths from one corner to the opposite corner with /maximum/ a dimension
 equal to the element's index i the list.
-}
cumulativeCuboids :: [Int]
cumulativeCuboids = scanl (+) 0 $ cuboidsOfDimension <$> [0..]

{- |
 Gets the unique number of cuboids with an integral shortest path from one
 corner to the opposite corner with at least one of the dimensions of the
 specified length.
-}
cuboidsOfDimension :: Int -> Int
cuboidsOfDimension a = length
  [ (a,b,c)
  | b <- [1..a]
  , c <- [1..b]
  , isSquare' ((b+c)*(b+c) + a*a)
  ]
