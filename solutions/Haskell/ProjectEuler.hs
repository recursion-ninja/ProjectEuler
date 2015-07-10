module ProjectEuler
  ( ProblemNumber()
  , answer
  , description
  ) where

import Safe
import Data.Ratio
import Data.Map
import Prelude hiding (lookup)

import qualified ProjectEuler.Problem_0001.Answer as P0001

newtype ProblemNumber = ProblemNumber Int
  deriving (Eq,Ord,Read,Show)

instance Enum ProblemNumber where
  fromEnum (ProblemNumber x) = x 
  toEnum x
    | x < min   = ProblemNumber $ ((x-1) `mod` max) + 1 
    | x > max   = ProblemNumber $ ((x-1) `mod` max) + 1
    | otherwise = ProblemNumber x
    where 
      max = fromEnum (maxBound :: ProblemNumber)
      min = fromEnum (minBound :: ProblemNumber)

instance Bounded ProblemNumber where
  minBound = ProblemNumber 1
  maxBound = ProblemNumber 522

answer :: ProblemNumber -> Maybe String
answer = (`lookup` answers)

answers :: Map ProblemNumber String
answers = fmap show 
        . mapKeys toEnum 
        $ fromList 
        [ (1,P0001.answer)
        ]

description :: ProblemNumber -> Maybe String
description = (`lookup` descriptions)

descriptions :: Map ProblemNumber String
descriptions =
  fromList $
  [
  ] 
{-
awardCompleted :: Award -> Bool
awardCompleted x =
  |  
  where progress awardProgress x
-}

