module Main
  ( main
  ) where

import qualified ProjectEuler.Problem_0001.Test as P0001

main :: IO ()
main = sequence_ testSuite

testSuite :: [IO ()]
testSuite = 
  [ 1 .:. P0001.main
  ]
  where 
    (.:.) :: Int -> IO () -> IO ()
    n .:. m = putStr "Problem " >> print n >> m
