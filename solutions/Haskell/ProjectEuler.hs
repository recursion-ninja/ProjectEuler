module ProjectEuler
  ( ProblemNumber()
  , answer
  , description
  ) where

import Safe
import Data.Ratio
import Data.Map
import Data.Maybe
import Data.Monoid
import Prelude hiding (lookup)

import qualified ProjectEuler.Problem_0001 as P0001

newtype ProblemNumber = ProblemNumber Int
  deriving (Eq,Ord,Read)

instance Bounded ProblemNumber where
  minBound = ProblemNumber 1
  maxBound = ProblemNumber 522

instance Enum ProblemNumber where
  fromEnum (ProblemNumber x) = x 
  toEnum x
    | x < min   = ProblemNumber $ ((x-1) `mod` max) + 1 
    | x > max   = ProblemNumber $ ((x-1) `mod` max) + 1
    | otherwise = ProblemNumber x
    where 
      max = fromEnum (maxBound :: ProblemNumber)
      min = fromEnum (minBound :: ProblemNumber)

instance Show ProblemNumber where
  show (ProblemNumber x) = show x

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

delegateMain :: ProblemNumber -> IO ()
delegateMain n
  | isJust main' = fromJust main'
  | otherwise    = return ()
  where
    main' = n `lookup` mains

mains :: Map ProblemNumber (IO ())
mains =
  mapKeys toEnum 
  . fromList $
  [ (1, P0001.main)
  ] 

