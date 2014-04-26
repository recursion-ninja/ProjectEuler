{--
 - Use Inclusion-Exclusion principle
 - Along with closed form calculation
 - To calculate to arbitrarily large bound
 - with arbirtarily many divisors
 --}

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Set (elems, fromList)
import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print $ eulerP0001 (getLimit args) (getDivisors args)

getLimit :: [String] -> Integer
getLimit args =
  if   length args  > 0
  then read $ args !! 0
  else 1000 --default

getDivisors :: [String] -> [Integer]
getDivisors args =
  if   length args > 1 && (not $ null list)
  then map read list
  else [3,5] --default
    where {-- Less fragile then standard read --} 
      list = parseList $ args !! 1
      parseList = filter (not . null) . splitRegex (mkRegex ",") . delete '[' . delete ']'

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> <divisors>\n"
         ++ "  Calculates the sum of all natural numbers less then <limit>\n"
         ++ "  and also divisible by a number in <divisors>\n"
         ++ "    <limit>    :: Int (1000)\n"
         ++ "    <divisors> :: CSV Int List (3,5)\n")

{-|-}

eulerP0001 :: Integral a => a -> [a] -> a
eulerP0001 l xs =
  uncurry (-) $ getMinuendSubtrahend (l-1) (reduce . elems $ fromList xs)

getMinuendSubtrahend :: (Integral a) => a -> [a] -> (a,a)
getMinuendSubtrahend l =
   both (sum . map (naturalSumModulo l . product)) . inclusionExclusion

inclusionExclusion :: [b] -> ([[b]],[[b]])
inclusionExclusion xs =
    both (concatMap (choose xs))
    . (
    (,)
    <$>  filter odd 
    <*>  filter even
    ) $  (enumFromTo 1 . length) xs
    where 
      choose :: [b] -> Int -> [[b]]
      _      `choose` 0 = [[]]
      []     `choose` _ =  []
      (x:xs) `choose` k =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

both :: (a->b) -> (a,a) -> (b,b)
both f = f *** f

reduce :: (Integral a) => [a] -> [a]
reduce []     = []
reduce (x:xs) = (x:) . reduce $ filter ((/=0).mod x) xs

naturalSumModulo :: (Integral a) => a -> a -> a
naturalSumModulo l x = x*n*(n+1) `div` 2
  where n = l `div` x

