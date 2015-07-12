module ProjectEuler
  ( ProblemNumber()
  , answer
  , description
  , solution
  ) where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Data.Map
import Data.Maybe
import Prelude hiding      (lookup)

import qualified ProjectEuler.Problem_0001 as P0001

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
  fromList
  [
  ] 

solution :: ProblemNumber -> IO ()
solution n
  | isJust main' = fromJust main'
  | otherwise    = print (Nothing :: Maybe String)
  where
    main' = n `lookup` mains

mains :: Map ProblemNumber (IO ())
mains =
  mapKeys toEnum 
  . fromList $
  [ (1, P0001.main)
  ] 

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
