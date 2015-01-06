import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Notes:
 - Rely on lazy evaluation
 - and generate fibonacci numbers tail recursively
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else 
    let limit = getLimit args
    in  print . sum . filter even $ takeWhile (<=limit) fibonacciSequence

getLimit :: [String] -> Integer
getLimit args =
  if   not  $ null args
  then read $ head args
  else 4000000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the sum of all even fibonacci numbers\n"
         ++ "  whose value is less then <limit>\n"
         ++ "    <limit>    :: Int (1000)\n")

{-!-}

fibonacciSequence :: [Integer]
fibonacciSequence = 
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)


