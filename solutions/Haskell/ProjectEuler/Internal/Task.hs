module ProjectEuler.Internal.Task
  ( parseTask
  , Task(..)
  , Operation(..)
  ) where

import Control.Applicative       (liftA2)
import Data.Char                 (isSpace)
import Data.Maybe
import Prelude hiding            (lookup)
import Safe                      (readMay)

import ProjectEuler.Internal.ProblemNumber

parseTask :: [String] -> (Maybe Task, [String])
parseTask       [] = (Nothing,  [])
parseTask      [x] = (Nothing, [x])
parseTask (x:y:zs) 
  |  isJust operation
  && isJust problemNum = (liftA2 Task operation problemNum, zs)
  | otherwise          = (Nothing, x:y:zs)
  where 
    operation  = readMay x 
    problemNum = readMay y

data Task = Task Operation ProblemNumber
  deriving (Eq,Read,Show)

data Operation = Answer | Description | Solution
  deriving (Eq,Enum,Show)

instance Read Operation where
  readsPrec _ str = readIt $ dropWhile isSpace str

readIt :: String -> [(Operation,String)]
readIt ('a':'n':'s':'w':'e':'r':xs)                     = [(Answer, xs)]
readIt ('A':'n':'s':'w':'e':'r':xs)                     = [(Answer, xs)]
readIt ('d':'e':'s':'c':'r':'i':'p':'t':'i':'o':'n':xs) = [(Description, xs)]
readIt ('D':'e':'s':'c':'r':'i':'p':'t':'i':'o':'n':xs) = [(Description, xs)]
readIt ('s':'o':'l':'u':'t':'i':'o':'n':xs)             = [(Solution, xs)]          
readIt ('S':'o':'l':'u':'t':'i':'o':'n':xs)             = [(Solution, xs)]
readIt _                                                = []
