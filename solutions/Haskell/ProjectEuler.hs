module ProjectEuler
  ( ProblemNumber()
  , answer
  , description
  , solution
  ) where

import Data.Map
import Data.Maybe
import Prelude hiding      (lookup)

import ProjectEuler.Internal.ProblemNumber
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
