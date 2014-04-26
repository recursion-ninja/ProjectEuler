import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit    = getLimit    args
        divisors = getDivisors args
    in
    print $ sum [ x | x <- [1 .. limit-1], divisors `can_divide` x]

getLimit :: [String] -> Int
getLimit args =
  if   length args  > 0
  then read $ args !! 0
  else 1000 --default

getDivisors :: [String] -> [Int]
getDivisors args =
  if   length args > 1
  then map read . filter (not . null) . nub $ splitRegex (mkRegex ",") $ args !! 1
  else [3,5] --default

can_divide :: [Int] -> Int -> Bool
infix 7  `can_divide`
divisors `can_divide` n = 
  foldr (\x y -> y || n `mod` x == 0) False divisors

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> <divisors>\n"
         ++ "  Calculates the sum of all natural numbers less then <limit>\n"
         ++ "  and also divisible by a number in <divisors>\n"
         ++ "    <limit>    :: Int (1000)\n"
         ++ "    <divisors> :: CSV Int List (3,5)\n")
