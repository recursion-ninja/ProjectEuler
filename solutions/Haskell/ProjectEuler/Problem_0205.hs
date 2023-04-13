{-# LANGUAGE DoAndIfThenElse #-}
module ProjectEuler.Problem_0205
  ( answer
  , description
  , solution
  , main
  ) where

import System.Environment (getProgName)

import ProjectEuler.Internal.Parameters   (getParameters,printHelpParamPassed)
import ProjectEuler.Problem_0205.Solution (solution)

answer :: Int
answer = solution

description :: String
description
  = unlines                                                              
  [ "In fact, by carefully choosing the digits on both cubes it is possible to"
  , "display all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49,"
  , "64, and 81."
  , ""
  , "For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on "
  , "one cube and {1, 2, 3, 4, 8, 9} on the other cube."
  , ""
  , "However, for this problem we shall allow the 6 or 9 to be turned upside-down so"
  , "that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for"
  , "all nine square numbers to be displayed; otherwise it would be impossible to"
  , "obtain 09."
  , ""
  , "In determining a distinct arrangement we are interested in the digits on each"
  , "cube, not the order."
  , "{1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}"
  , "{1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}"
  , ""
  , "But because we are allowing 6 and 9 to be reversed, the two distinct sets in the"
  , "last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the"
  , "purpose of forming 2-digit numbers."
  , ""
  , "How many distinct arrangements of the two cubes allow for all of the square"
  , "numbers to be displayed?"
  ]
    
main :: IO ()
main = do
  args <- getParameters
  if   printHelpParamPassed args
  then printHelp
  else print solution

printHelp :: IO ()
printHelp
    = getProgName
  >>= \name -> putStrLn $ unlines
    [ ""
    , "  Usage: " ++ name
    , "  All unique dice assignments which can display the square numbers less than 100"
    ]
