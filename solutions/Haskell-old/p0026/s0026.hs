import Data.List                 (maximumBy)
import Data.Maybe                (isJust,fromJust)
import Data.Ord                  (comparing)
import Data.Sequence             (fromList,index,update,Seq)
import Math.NumberTheory.Powers  (powerMod)
import Math.NumberTheory.Primes  (primes,factorise')
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Only prime a number will have longest repeating fraction
 - Note the maximum sequence length for n is n-1
 - Note if n is prime and b is a primative root of p
 - Then the repeating decimal expansion length is p-1
 - Look from upperbound until we find n S.T. len(n) = n-1
 - If we fail to find such an n,
 - Then exaustively search with the naive algorithm
 - This is ok because failing to find such an n
 - Implies the upper bound is small
 -}

type FractionExpansion = (Int,([Int],[Int]))

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args
        base  = getBase  args
    in  putStrLn . showAnswer $ maxFractionExpansion base limit

getLimit :: [String] -> Int
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 1000 --default

getBase :: [String] -> Int
getBase args =
  if   length args >  1
  then read $ args !! 1
  else 10 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

showAnswer :: Maybe FractionExpansion -> String
showAnswer x
  | isJust x  = show . fst $ fromJust x
  | otherwise = "(none)"

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> <base>\n"
         ++ "  Calculates the denominator of the unit fraction \n"
         ++ "  with the longest repeaing decimal expansion  \n"
         ++ "  with the denomination less than <limit>. \n"
         ++ "    <limit>  :: Int (1000)\n"
         ++ "    <base>   :: Int (10)\n")

{-!-}

maxFractionExpansion :: Int -> Int -> Maybe FractionExpansion
maxFractionExpansion base limit
  | isJust root     = Just . fractionExpansion base $ fromJust root
  | not $ null list = Just biggest
  | otherwise       = Nothing
  where
    root    = largestPrimativeRoot base limit
    biggest = maximumBy (comparing (length.snd.snd)) list
    list    = naiveExpansions base limit

largestPrimativeRoot :: Int -> Int -> Maybe Int
largestPrimativeRoot base limit
  | base  <= 1          = Nothing
  | limit <  0          = largestPrimativeRoot base $ abs limit
  | null primativeRoots = Nothing
  | otherwise           = Just maxExpansion
  where
    maxExpansion   = head primativeRoots
    primativeRoots = filter (isPrimativeRoot base) . reverse
                   . takeWhile (<limit) $ map fromIntegral primes

isPrimativeRoot :: Int -> Int -> Bool
isPrimativeRoot base n = isPrimative
  where
    isPrimative = isCoprime && (null $ filter (==1) powers)
    isCoprime   = gcd base n == 1
    powers      = map ((~^~) powerMod b' n' . (div phi)) factors
    factors     = map fst $ factorise' phi
    phi         = pred n'
    n'          = fromIntegral n
    b'          = fromIntegral base
    (~^~)       = flip .: (flip .) flip
    (.:)        = (.).(.)

naiveExpansions :: Int -> Int -> [FractionExpansion]
naiveExpansions base limit
  | base  <= 1 = []
  | limit <= 1 = []
  | otherwise  = map (fractionExpansion base) . enumFromTo 1 $ pred limit

fractionExpansion :: Int -> Int -> FractionExpansion
fractionExpansion base x = fractionExpansion' 1 [] (cleanTally x) x
  where
    cleanTally = fromList . flip replicate (-1)
    fractionExpansion' :: Int -> [Int] -> Seq Int -> Int -> FractionExpansion
    fractionExpansion' remainder stack seen denom
      | r == 0    = (denom,(reverse $ q:stack, []))
      | isCycle   = (denom, answer q (stack) [])
      | otherwise = fractionExpansion' r (q:stack) seen' denom 
      where
        (q,r)   = (remainder * base) `quotRem` denom
        seen'   = update r q seen
        isCycle = seen `index` r == q
        answer e xs ys
         | null xs   = ([],  ys)
         | z == e    = (reverse zs, z:ys)
         | otherwise = answer e zs (z:ys) 
         where
           (z:zs) = xs

