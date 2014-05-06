import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike
import Math.NumberTheory.Primes.Sieve

{--
 - Notes:
 - Use library packages like a (> PRO <)
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else 
    let limit = getLimit args 
    in  print . sum . takeWhile (<limit) $ primes

getLimit :: [String] -> Integer
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 2000000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Calculates the sum all primes less then <limit>\n"
         ++ "    <limit>  :: Integer (2000000)\n")
