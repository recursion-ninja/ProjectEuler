{--
 - Notes:
 - Use closed forms:
 - Square of natural summation: (n(n+1)/2)^2
 - Square pyramidal number: n(n+1)(2n+1)/6
 - Difference can be reduced to:
 - ( n(3n-2)(n+1)^2 )/12
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print . reducedClosedForm $ getLimit args

getLimit :: [String] -> Integer
getLimit args =
  if   length args  > 0
  then read $ args !! 0 
  else 100 --default

reducedClosedForm :: Integer -> Integer
reducedClosedForm n = 
  ( n*(n-1)*(n+1)*(3*n+2) ) `div` 12

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the square of the sum of natural numbers from 0 to <limit>\n"
         ++ "  minus the sum of the squares of natural numbers from 0 to <limit>\n"
         ++ "    <limit>  :: Int (100)\n")
