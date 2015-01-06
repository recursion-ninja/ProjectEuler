import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Notes:
 - Answer is least common multiple of all numbers in range
 -}

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else 
    let lower = getLower args
        upper = getUpper args
    in  print $ foldr1 lcm [lower..upper]

getLower :: [String] -> Int
getLower args =
  if   not $  null args
  then max 1 (read $ head args)
  else 1 --default

getUpper :: [String] -> Int
getUpper args =
  if   length args > 1
  then max 1 (read $ args !! 1)
  else 20 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <lower> <upper>\n"
         ++ "  Calculates the least number evenly divisible\n"
         ++ "  by all numbers in the range from <lower> to <upper>\n"
         ++ "    <lower>  :: Int (1)"
         ++ "    <upper>  :: Int (20)\n")
