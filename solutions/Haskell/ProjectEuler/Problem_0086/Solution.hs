module ProjectEuler.Problem_0086.Solution
  ( solution
  , alpha
  , beta
  , gamma
  ) where

import Math.NumberTheory.Powers.Squares 

data PythagoreanTriple 
   = PT
   { tripleMin  :: Integer
   , tripleMean :: Integer
   , tripleMax  :: Integer
   } deriving (Eq,Ord)

instance Show PythagoreanTriple where
  show (PT a b c) = mconcat [ "(", show a
                            , ",", show b
                            , ",", show c
                            , ")"
                            ]

solution :: Int -> Integer
solution = (gamma !!) --const 0

gamma :: [Integer]
gamma = scanl (+) 0 $ beta . alpha <$> [0..]

beta :: [PythagoreanTriple] -> Integer
beta = sum . fmap cuboidRoutes
  where
    cuboidRoutes pt = tripleMean pt - tripleMin pt + 1
    {-
      | even m    = m `div` 2
      | otherwise = m `div` 2 + 1
      where
        m = 
      -}
                      
alpha :: Integer -> [PythagoreanTriple]
alpha n = filterTriples
          [ (n, x, exactSquareRoot $ n*n + x*x)
          | x <- [n..2*n]
          ]
  where
    filterTriples :: [(Integer,Integer,Maybe Integer)] -> [PythagoreanTriple]
    filterTriples    []  = []
    filterTriples (x:xs) =
      case x of
        (_, _, Nothing) -> result
        (a, b, Just c ) -> PT { tripleMin  = a
                              , tripleMean = b
                              , tripleMax  = c
                              } : result
      where
        result = filterTriples xs
