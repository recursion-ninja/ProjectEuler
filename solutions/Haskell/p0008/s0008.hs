{--
 - Notes:
 - Straight Forward
 -}

import System.Environment
import Data.List
import Data.Char
import Text.Regex
import Text.Regex.Base.RegexLike

main = do
  args <- getArgs
  name <- getProgName
  info <- getContents
  if   printHelpParamPassed args
  then printHelp name
  else 
    let digits    = getDigits args
        cleanData = map digitToInt $ filter (isDigit) info
    in  print . maximum $ map (product . take 5) (tails cleanData)

getDigits :: [String] -> Integer
getDigits args =
  if   length args  > 0
  then read $ args !! 0 
  else 5 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <digits>\n"
         ++ "  Calculates the largest product of <digits> consecutive digits\n"
         ++ "  from the decimal number read from STDIN\n"
         ++ "    <digits>  :: Int (5)\n")
