import System.Environment        (getArgs,getProgName)
import Data.List                 (tails)
import Data.Char                 (digitToInt,isDigit)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Notes:
 - Straight Forward
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  info <- getContents
  if   printHelpParamPassed args
  then printHelp name
  else 
    let digits    = getDigits args
        cleanData = map digitToInt $ filter isDigit info
    in  print . maximum . map (product . take digits) $ tails cleanData

getDigits :: [String] -> Int
getDigits args =
  if   not  $ null args
  then read $ head args 
  else 5 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <digits>\n"
         ++ "  Calculates the largest product of <digits> consecutive digits\n"
         ++ "  from the decimal number read from STDIN\n"
         ++ "    <digits>  :: Int (5)\n")
