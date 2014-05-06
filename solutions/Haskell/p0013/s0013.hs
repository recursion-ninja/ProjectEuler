import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  info <- getContents
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args 
    in  putStrLn . prefix limit $ sumAll info

getLimit :: [String] -> Int
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 10 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Reads base 10 integers from STDIN line by line\n"
         ++ "  calculates the sumation of all integers and\n"
         ++ "  displays the first <limit> digits of the sum\n"
         ++ "    <limit>  :: Int (10)\n")

{-!-}

sumAll :: String -> Integer
sumAll = sum . map read . lines

prefix :: Int -> Integer -> String
prefix lim = take lim . show 
