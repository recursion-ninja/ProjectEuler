{-# LANGUAGE DoAndIfThenElse #-}
module ProjectEuler.Problem_0001
  ( answer
  , description
  , solution
  , main
  ) where

import Data.List                 (delete)
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex,splitRegex)
import Text.Regex.Base.RegexLike (match)
import ProjectEuler.Problem_0001.Solution (solution)

defaultLimit    = 1000
defaultDivisors = [3,5]

answer :: Integer
answer = solution defaultLimit defaultDivisors

description :: String
description
  = unlines
  [ "If we list all the natural numbers below 10 that are multiples of 3 or 5, "
  , "we get 3, 5, 6 and 9. The sum of these multiples is 23."
  , ""
  , "Find the sum of all the multiples of 3 or 5 below 1000."
  ]

main :: IO ()
main = do
  args <- getArgs
  if   printHelpParamPassed args
  then printHelp
  else print $ solution (getLimit args) (getDivisors args)

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

printHelp :: IO ()
printHelp = getProgName
        >>= \name -> putStrLn
          $ "\n" ++ unlines
          [ "  Usage: " ++ name ++ " <limit> <divisors>"
          , "  Calculates the sum of all natural numbers less then <limit>"
          , "  and also divisible by a number in <divisors>"
          , "    <limit>    :: Int (1000)"
          , "    <divisors> :: CSV Int List (3,5)"
          ]
