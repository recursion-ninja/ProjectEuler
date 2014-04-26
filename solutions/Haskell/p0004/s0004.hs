{--
 - Notes:
 - Use library packages like a pro
 -}

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
    let (lower,upper) = getRange $ getDigits args
    in  print $ head . reverse . sort $ [ x*y | x <- [lower..upper], y <- [lower..upper], reverse (show(x*y)) == (show (x*y))]
--    in  print $ head $ dropWhile (not . isPalindrome) [ x*y | x <- [upper,upper-1..lower], y <- [upper,upper-1..lower] ]
--    in  print $ head $ dropWhile (not . isPalindrome) $ fMerge (*) [upper,upper-1..lower] [upper,upper-1..lower]
--    in  print $ [ x*y | x <- [upper,upper-1..lower], y <- [upper,upper-1..x] ]
--    in  print $ head $ dropWhile (not . isPalindrome) [ x*y | x <- [upper,upper-1..lower], y <- [upper,upper-1..x] ]
--    in  print $ sort $ filter (isPalindrome) [ x*y | x <- [upper,upper-1..lower], y <- [upper,upper-1..x] ]

isPalindrome :: Int -> Bool
isPalindrome n = reverse (show n) == (show n)

--fmerge :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
fMerge g [] _      = []
fMerge g _ []      = []
fMerge g (x:xs) (y:ys) = (g x y) : (fMerge g xs ys)

getDigits :: [String] -> Int
getDigits args =
  if   length args  > 0
  then read $ args !! 0 
  else 3 --default

getRange :: Int -> (Int,Int)
getRange digits =  ( 10^(digits-1), (10^digits)-1 )

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the largest prime factor of <target>\n"
         ++ "    <target>  :: Int (600851475143)\n")
