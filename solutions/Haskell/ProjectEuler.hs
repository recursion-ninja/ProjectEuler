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
import qualified ProjectEuler.Problem_0002 as P0002
import qualified ProjectEuler.Problem_0003 as P0003
import qualified ProjectEuler.Problem_0086 as P0086
import qualified ProjectEuler.Problem_0090 as P0090

answer :: ProblemNumber -> Maybe String
answer = (`lookup` answers)

answers :: Map ProblemNumber String
answers
  = mapKeys toEnum 
  $ fromList 
  [ ( 1, show $ P0001.answer)
  , ( 2, show $ P0002.answer)
  , ( 3, show $ P0003.answer)
  , (86, show $ P0086.answer)
  , (90, show $ P0090.answer)
  ]

description :: ProblemNumber -> Maybe String
description = (`lookup` descriptions)

descriptions :: Map ProblemNumber String
descriptions
  = mapKeys toEnum 
  $ fromList
  [ ( 1, P0001.description)
  , ( 2, P0002.description)
  , ( 3, P0003.description)
  , (86, P0086.description)
  , (90, P0090.description)
  ] 

solution :: ProblemNumber -> IO ()
solution n
  | isJust main' = fromJust main'
  | otherwise    = print (Nothing :: Maybe String)
  where
    main' = n `lookup` mains

mains :: Map ProblemNumber (IO ())
mains
  = mapKeys toEnum 
  . fromList $
  [ ( 1, P0001.main)
  , ( 2, P0002.main)
  , ( 3, P0003.main)
  , (86, P0086.main)
  , (90, P0090.main)
  ]
