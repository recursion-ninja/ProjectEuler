{--
 - Notes:
 - Use library packages like a (> PRO <)
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike

main = do
  args <- getArgs
  name <- getProgName
  info <- getContents
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args 
    in  print . take limit $ show . sum . map read $ lines $ info

getLimit :: [String] -> Int
getLimit args =
  if   length args  > 0
  then read $ args !! 0 
  else 10 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit>\n"
         ++ "  Reads base 10 integers from STDIN line by line\n"
         ++ "  calculates the sumation of all integers\n"
         ++ "  and displays the first <limit> digits of the sum\n"
         ++ "    <limit>  :: Int (10)\n")
