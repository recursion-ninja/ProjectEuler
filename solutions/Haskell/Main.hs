module Main
  ( main
  ) where

import Data.Maybe
import Prelude hiding      (lookup)
import System.Environment  (getArgs,getProgName)

import ProjectEuler
import ProjectEuler.Internal.Task

main :: IO ()
main = getArgs
   >>= handleTask . parseTask

handleTask :: (Maybe Task, [String]) -> IO ()
handleTask (Nothing, _)                   = printUsage
handleTask (Just (Task Answer      n), _) = printMaybe $ answer n
handleTask (Just (Task Description n), _) = printMaybe . fmap ("\n"++) $ description n
handleTask (Just (Task Solution    n), _) = solution n

printMaybe :: Maybe String -> IO ()
printMaybe (Just str) = putStrLn $ "Just: " ++ str
printMaybe Nothing    = putStrLn   "Nothing"

printUsage :: IO ()
printUsage = do 
  name <- getProgName
  putStrLn $ unlines
    [ "usage: " ++ name ++ " Operation ProblemNumber [Arguments]"
    , ""
    , "  Operation:      Answer | Description | Solution"
    , "  ProblemNumber:  Int in [" 
                               ++ (show . fromEnum) (minBound::ProblemNumber)
                               ++ ".."
                               ++ (show . fromEnum) (maxBound::ProblemNumber)
                               ++ "]"
    , "  Arguments:      --help to see specific argument options"
    , "                         for the given Operation & ProblemNumber"
    ]
