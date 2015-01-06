import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let rows  = getRows args 
        cols  = getCols args 
    in  print $ totalPaths rows cols

getRows :: [String] -> Int
getRows args =
  if   not  $ null args
  then read $ head args 
  else 20 --default

getCols :: [String] -> Int
getCols args =
  if   length args  > 2
  then read $ args !! 2 
  else 20 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <rows> <cols> \n"
         ++ "  Calculates the sum of number of paths which exist \n"
         ++ "  to travel from one corner to the opposite corner  \n"
         ++ "  of a <rows> by <cols> grid. \n"
         ++ "    <rows>  :: Int (20)\n"
         ++ "    <cols>  :: Int (20)\n")

{-!-}

totalPaths :: Int -> Int -> Integer
totalPaths rows cols = numerator `div` denominator
  where
    numerator   = factorial (rows + cols)
    denominator = factorial  rows * factorial cols
    factorial   = product . enumFromTo 1 . fromIntegral
