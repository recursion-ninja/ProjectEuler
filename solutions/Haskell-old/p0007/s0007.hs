import Math.NumberTheory.Primes.Counting (nthPrime)
import System.Environment                (getArgs,getProgName)
import Text.Regex                        (mkRegex)
import Text.Regex.Base.RegexLike         (match)

{--
 - Notes:
 - Use library packages like a pro
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print . nthPrime $ getTarget args

getTarget :: [String] -> Integer
getTarget args =
  if   not  $ null args
  then read $ head args 
  else 10001 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the <target>th prime number\n"
         ++ "    <target>  :: Integer (10001)\n")
