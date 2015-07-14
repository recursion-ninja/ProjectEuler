{-# LANGUAGE DoAndIfThenElse #-}
module ProjectEuler.Problem_0003
  ( answer
  , description
  , solution
  , main
  ) where

import System.Environment (getProgName)

import ProjectEuler.Internal.Parameters   (getParameters, printHelpParamPassed)
import ProjectEuler.Problem_0003.Solution (solution)

{--
 - Notes:
 - Use library packages like a pro
 -}

defaultNumber :: Integer
defaultNumber = 600851475143

answer :: Integer
answer = solution defaultNumber

description :: String
description
  = unlines
  [ "The prime factors of 13195 are 5, 7, 13 and 29."
  , ""
  , "What is the largest prime factor of the number 600851475143 ?"
  ]

main :: IO ()
main = do
  args <- getParameters
  if   printHelpParamPassed args
  then printHelp
  else print . solution . getNumber $ args

getNumber :: [String] -> Integer
getNumber args =
  if   not  $ null args
  then read $ head args
  else defaultNumber

printHelp :: IO ()
printHelp
    = getProgName
  >>= \name -> putStrLn $ unlines
    [ ""
    , "  Usage: " ++ name ++ " <target>"
    , "  Calculates the largest prime factor of <target>"
    , "    <target> " ++ show defaultNumber ++ " :: Integer"
    ]
