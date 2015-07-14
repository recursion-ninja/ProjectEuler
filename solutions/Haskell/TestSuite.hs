module Main
  ( main
  ) where

import qualified ProjectEuler.Problem_0001.Test as P0001
import qualified ProjectEuler.Problem_0002.Test as P0002

main :: IO ()
main = sequence_ testSuite

testSuite :: [IO ()]
testSuite = 
  [ 1 .:. P0001.main
  , 2 .:. P0002.main
  ]
  where 
    (.:.) :: Int -> IO () -> IO ()
    n .:. m = putStr "Problem " >> print n >> m
