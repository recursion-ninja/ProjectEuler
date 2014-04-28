{--
 - Notes:
 - Use library packages like a pro
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
    let (lower,upper) = getRange $ getDigits args
    in  print $ maximum [ x*y | x <- [lower..upper], y <- [lower..upper], isPalindrome (x*y)]

getDigits :: [String] -> Int
getDigits args =
  if   not  $ null args
  then read $ head args 
  else 3 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the largest prime factor of <target>\n"
         ++ "    <target>  :: Int (600851475143)\n")

isPalindrome :: Int -> Bool
isPalindrome n = reverse (show n) == show n

getRange :: Int -> (Int,Int)
getRange digits =  ( 10^(digits-1), (10^digits)-1 )

