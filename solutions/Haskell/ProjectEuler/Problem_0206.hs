{-# LANGUAGE DoAndIfThenElse #-}
module Main
  ( answer
  , description
  , solution
  , main
  ) where

import Data.Maybe (fromMaybe)
--import Numeric.Natural
import System.Environment (getArgs)

--import ProjectEuler.Internal.Parameters   (getParameters, printHelpParamPassed)
import ProjectEuler.Problem_0206.Solution (FactorSet, solution)


answer :: FactorSet
answer = fromMaybe mempty . solution $ read "1_2_3_4_5_6_7_8_9_0"


description :: String
description
  = unlines
  [
  ]


main :: IO ()
main = do
    args <- getArgs
    maybe (putStrLn "No solution exists") print . solution . read $ args !! 0


{-
printHelp :: IO ()
printHelp = getProgName >>= \name -> putStrLn $ unlines
    [ ""
    , "  Usage: " ++ name
    , "  All unique dice assignments which can display the square numbers less than 100"
    ]
-}
