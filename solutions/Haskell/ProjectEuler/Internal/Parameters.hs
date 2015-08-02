module ProjectEuler.Internal.Parameters
  ( (~!?)
  , getParameters
  , printHelpParamPassed
  ) where

import Control.Applicative        ((<$>))
import Data.Maybe                 (isJust)
import Safe                       (headMay,readMay)
import System.Environment         (getArgs)
import Text.Regex                 (mkRegex)
import Text.Regex.Base.RegexLike  (match)

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
    
printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

(~!?) :: Read a => [String] -> Int -> Maybe a
(~!?) [] _          = Nothing
(~!?)  _ n  | n < 0 = Nothing
(~!?) xs n          = headMay (drop n xs) >>= readMay
