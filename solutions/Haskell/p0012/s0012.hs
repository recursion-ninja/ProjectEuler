import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike
import Math.NumberTheory.Primes.Factorisation (factorise')

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args 
    in  print $ minTriangleNumberWithAtLeastDivisors limit

getLimit :: [String] -> Int
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 500 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <divisors> \n"
         ++ "  Calculates the first triangle number which is\n"
         ++ "  divisable by at least <divisors> numbers \n"
         ++ "    <limit>  :: Int (10)\n")

{-!-}

minTriangleNumberWithAtLeastDivisors :: Int -> Integer
minTriangleNumberWithAtLeastDivisors limit =
  fst . head $ dropWhile ((<=limit).snd) [ (x, divisorCount x) | x <- triangleNumbers ]
  where
    divisorCount    = product . map (succ.snd) . factorise' 
    triangleNumbers = scanl1 (+) [1..]
