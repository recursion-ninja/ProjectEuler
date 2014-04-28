{--
 - Notes:
 - Use library packages like a pro
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike
import Math.NumberTheory.Primes.Factorisation

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print . maximum . map fst . factorise $ getTarget args

getTarget :: [String] -> Integer
getTarget args =
  if   not  $ null args
  then read $ head args 
  else 600851475143 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the largest prime factor of <target>\n"
         ++ "    <target>  :: Integer (600851475143)\n")
