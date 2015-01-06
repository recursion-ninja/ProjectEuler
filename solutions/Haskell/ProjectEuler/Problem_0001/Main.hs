{-# LANGUAGE DoAndIfThenElse #-}
module Main
 ( main
 ) where

import Data.List                 (delete)
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex,splitRegex)
import Text.Regex.Base.RegexLike (match)
import qualified ProjectEuler.Problem_0001.Solution as P0001

{--
 - Use Inclusion-Exclusion principle
 - Along with closed form calculation
 - To calculate to arbitrarily large bound
 - with arbirtarily many divisors
 --}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print $  P0001.solution (getLimit args) (getDivisors args)

getLimit :: [String] -> Integer
getLimit args =
  if   not  $ null args
  then read $ head args
  else 1000 --default

getDivisors :: [String] -> [Integer]
getDivisors args =
  if   length args > 1 && (not . null) list
  then map read list
  else [3,5] --default
    where -- Less fragile then standard read 
      list = parseList $ args !! 1
      parseList = filter (not . null) . splitRegex (mkRegex ",") . delete '[' . delete ']'

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> <divisors>\n"
         ++ "  Calculates the sum of all natural numbers less then <limit>\n"
         ++ "  and also divisible by a number in <divisors>\n"
         ++ "    <limit>    :: Int (1000)\n"
         ++ "    <divisors> :: CSV Int List (3,5)\n")
