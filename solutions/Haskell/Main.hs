module Main
  ( main
  ) where

import Control.Applicative (liftA2)
import Data.Char           (isSpace)
import Data.Maybe
import Prelude hiding      (lookup)
import Safe                (readMay)
import System.Environment  (getArgs,getProgName)

import ProjectEuler

main :: IO ()
main = getArgs
   >>= handleTask . parseTask

parseTask :: [String] -> (Maybe Task, [String])
parseTask       [] = (Nothing,  [])
parseTask      [x] = (Nothing, [x])
parseTask (x:y:zs) 
  |  isJust operation
  && isJust problemNum = (liftA2 Task operation problemNum, zs)
  | otherwise          = (Nothing, (x:y:zs))
  where 
    operation  = readMay x 
    problemNum = readMay y

handleTask :: (Maybe Task, [String]) -> IO ()
handleTask (Nothing, _)              = printUsage
handleTask (Just (Task Answer      n), _) = print $ answer n
handleTask (Just (Task Description n), _) = print $ description n
handleTask (Just (Task Solution    n), _) = solution n

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
    , "                         for the given Operation & ProbelmNumber"
    ]

data Task = Task Operation ProblemNumber
  deriving (Eq,Read,Show)

data Operation = Answer | Description | Solution
  deriving (Eq,Enum,Show)

instance Read Operation where
  readsPrec _ str = readIt $ dropWhile isSpace str
    where
readIt :: String -> [(Operation,String)]
readIt ('a':'n':'s':'w':'e':'r':xs)                     = [(Answer, xs)]
readIt ('A':'n':'s':'w':'e':'r':xs)                     = [(Answer, xs)]
readIt ('d':'e':'s':'c':'r':'i':'p':'t':'i':'o':'n':xs) = [(Description, xs)]
readIt ('D':'e':'s':'c':'r':'i':'p':'t':'i':'o':'n':xs) = [(Description, xs)]
readIt ('s':'o':'l':'u':'t':'i':'o':'n':xs)             = [(Solution, xs)]          
readIt ('S':'o':'l':'u':'t':'i':'o':'n':xs)             = [(Solution, xs)]
readIt _                                                = []
--  readsPrec _ _ = [(Description,"")]
