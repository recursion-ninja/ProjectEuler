module ProjectEuler.Internal.ProblemNumber
  ( ProblemNumber()
  ) where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))

newtype ProblemNumber = ProblemNumber Int
  deriving (Eq,Ord)

instance Bounded ProblemNumber where
  minBound = ProblemNumber 1
  maxBound = ProblemNumber 522

instance Enum ProblemNumber where
  fromEnum (ProblemNumber x) = x 
  toEnum x
    | x < min'  = ProblemNumber $ ((x-1) `mod` max') + 1 
    | x > max'  = ProblemNumber $ ((x-1) `mod` max') + 1
    | otherwise = ProblemNumber x
    where 
      max' = fromEnum (maxBound :: ProblemNumber)
      min' = fromEnum (minBound :: ProblemNumber)

instance Read ProblemNumber where
  readsPrec x str = (ProblemNumber *** id) <$> (readsPrec x str :: [(Int,String)])

instance Show ProblemNumber where
  show (ProblemNumber x) = show x
