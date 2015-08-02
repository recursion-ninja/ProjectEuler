{-# LANGUAGE DoAndIfThenElse #-}
module ProjectEuler.Problem_0001
  ( answer
  , description
  , solution
  , main
  ) where

import System.Environment        (getProgName)

import ProjectEuler.Internal.Parameters   ((~!?),getParameters,printHelpParamPassed)
import ProjectEuler.Problem_0001.Solution (solution)

defaultLimit :: Integer
defaultLimit    = 1000
defaultDivisors :: [Integer]
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
  args <- getParameters
  if   printHelpParamPassed args
  then printHelp
  else print $ solution (getLimit args) (getDivisors args)

getLimit :: [String] -> Integer
getLimit = maybe defaultLimit id . (~!? 0)

getDivisors :: [String] -> [Integer]
getDivisors = maybe defaultDivisors id . (~!? 1)

printHelp :: IO ()
printHelp
    = getProgName
  >>= \name -> putStrLn $ unlines
    [ ""
    , "  Usage: " ++ name ++ " <limit> <divisors>"
    , "  Calculates the sum of all natural numbers less then <limit>"
    , "  and also divisible by a number in <divisors>"
    , "    <limit>    1000  ::  Integer"
    , "    <divisors> [3,5] :: [Integer]"
    ]

