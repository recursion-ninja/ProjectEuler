{--
 - Notes:
 - Use library packages like a pro
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike
import Math.NumberTheory.Primes.Counting

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print . nthPrime $ getTarget args

getTarget :: [String] -> Integer
getTarget args =
  if   not  $ null args
  then read $ head args 
  else 10001 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the <target>th prime number\n"
         ++ "    <target>  :: Integer (10001)\n")
