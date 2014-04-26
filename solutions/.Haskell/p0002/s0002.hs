{--
 - Notes:
 - Rely on lazy evaluation
 - and generate fibonacci numbers tail recursively
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
  else 
    let limit = getLimit args
    in  print . sum . filter even $ takeWhile (<=limit) fibonacciSequence

getLimit :: [String] -> Integer
getLimit args =
  if   length args  > 0
  then read $ args !! 0 
  else 4000000 --default

fibonacciSequence :: [Integer]
fibonacciSequence = 
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the sum of all even fibonacci numbers\n"
         ++ "  whose value is less then <limit>\n"
         ++ "    <limit>    :: Int (1000)\n")
