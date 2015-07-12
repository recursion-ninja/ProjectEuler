module ProjectEuler.Internal.Parameters
  ( getParameters
  ) where

import Control.Applicative        ((<$>))
import Data.Maybe                 (isJust)
import System.Environment         (getArgs)
import ProjectEuler.Internal.Task (parseTask)

-- | Conditionally filtered program arguments
-- | Returns the original arguments if 
-- | the first 2 arguments cannot be parsed into a Task
-- | otherwsie returns the original arguments

getParameters :: IO [String]
getParameters = workingArgs <$> getArgs
  where
    workingArgs args = if isJust task then xs else args
      where
        (task, xs) = parseTask args
    
