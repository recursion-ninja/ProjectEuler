import Math.NumberTheory.Primes.Factorisation (factorise')
import System.Environment                     (getArgs,getProgName)
import Text.Regex                             (mkRegex)
import Text.Regex.Base.RegexLike              (match)

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
  else print . maximum . map fst . factorise' $ getTarget args

getTarget :: [String] -> Integer
getTarget args =
  if   not  $ null args
  then read $ head args 
  else 600851475143 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the largest prime factor of <target>\n"
         ++ "    <target>  :: Integer (600851475143)\n")
