import Data.List
import Data.Map ((!),fromAscList,toList)
import Data.Ord
import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike

{-- Memotize, but only under the limit --}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args 
    in  print $ collatzMaximumChain limit

getLimit :: [String] -> Int
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 1000000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> \n"
         ++ "  Calculates the number below <limit> \n"
         ++ "  which creates the longest Colatz Chain \n"
         ++ "    <limit>  :: Int (1000000)\n")

{-!-}

collatzMaximumChain :: Int -> Int
collatzMaximumChain =
  fst . maximumBy (comparing snd) . collatzLengths

collatzLengths :: Int -> [(Int,Int)]
collatzLengths k = toList lengthMap
  where
    lim = fromIntegral k
    lengthMap = fromAscList [(i, collatzLength $ fromIntegral i) | i <- [1..k]]
    collatzLength :: Integer -> Int
    collatzLength x
      | x == 1    = 0
      | y <= lim  = 1 + (lengthMap ! fromIntegral y)
      | otherwise = 1 +  collatzLength y
      where
        y = collatz x
        collatz n
          | even n    = n `div` 2 
          | otherwise = n * 3 + 1

