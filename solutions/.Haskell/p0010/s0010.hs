{--
 - Notes:
 - Use library packages like a (> PRO <)
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike
import Math.NumberTheory.Primes.Sieve

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else 
    let limit = getLimit args 
    in  print . sum . takeWhile (<limit) $ primes

getLimit :: [String] -> Integer
getLimit args =
  if   length args  > 0
  then read $ args !! 0 
  else 2000000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the sum all primes less then <limit>\n"
         ++ "    <limit>  :: Integer (2000000)\n")
