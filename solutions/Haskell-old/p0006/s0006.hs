--import Data.List
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Notes:
 - Use closed forms:
 - Square of natural summation: (n(n+1)/2)^2
 - Square pyramidal number:      n(n+1)(2n+1)/6
 - Difference can be reduced to:
 - ( n(3n-2)(n+1)^2 )/12
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args
    in  print $ reducedClosedForm limit

getLimit :: [String] -> Integer
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 100 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the square of the sum of natural numbers from 0 to <limit>\n"
         ++ "  minus the sum of the squares of natural numbers from 0 to <limit>\n"
         ++ "    <limit>  :: Integer (100)\n")

{-!-}

reducedClosedForm :: Integer -> Integer
reducedClosedForm n = 
  ( n*(n-1)*(n+1)*(3*n+2) ) `div` 12


