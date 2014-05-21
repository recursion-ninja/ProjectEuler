import Data.List                 (maximumBy)
import Data.Maybe                (isJust,fromJust)
import Data.Ord                  (comparing)
import Data.Sequence             (adjust,fromList,index,Seq)
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
    in  print . fst $ maxFractionExpansion base limit

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

maxFractionExpansion :: Int -> Int -> FractionExpansion
maxFractionExpansion base limit
  | isJust root = fractionExpansion base $ fromJust root
  | otherwise   = biggest
  where
    root    = largestPrimativeRoot base limit
    biggest = maximumBy (comparing (length.snd.snd)) 
            $ naiveExpansions base limit

largestPrimativeRoot :: Int -> Int -> Maybe Int
largestPrimativeRoot base limit
  | base  == 0          = Just 0
  | base  == 1          = Just 1
  | limit <  0          = largestPrimativeRoot base $ abs limit
  | null primativeRoots = Nothing
  | otherwise           = Just maxLength
  where 
    maxLength      = pred $ head primativeRoots
    primativeRoots = filter (isPrimativeRoot base) . reverse
                   . takeWhile (<limit) $ map fromIntegral primes

isPrimativeRoot :: Int -> Int -> Bool
isPrimativeRoot base n = isPrimative
  where
    isPrimative = null $ filter (==1) powers
    powers      = map ((~^~) powerMod b' n' . (div phi)) factors
    factors     = map fst $ factorise' phi
    phi         = pred n'
    n'          = fromIntegral n
    b'          = fromIntegral base
    (~^~)       = flip .: (flip .) flip
    (.:)        = (.).(.)

naiveExpansions :: Int -> Int -> [FractionExpansion]
naiveExpansions base = map (fractionExpansion base) . enumFromTo 2

fractionExpansion :: Int -> Int -> FractionExpansion
fractionExpansion base x = fractionExpansion' 1 [] (cleanTally x) x
  where
    cleanTally = fromList . flip replicate True
    fractionExpansion' :: Int -> [Int] -> Seq Bool -> Int -> FractionExpansion
    fractionExpansion' remainder stack tally denom
      | r == 0             = (denom,(stack,[]))
      | (tally' `index` r) = (denom,answer r stack [])
      | otherwise          = fractionExpansion' r (q:stack) tally' denom 
      where
        (q,r)  = (remainder * base) `quotRem` denom
        tally' = adjust not r tally 
        answer e xs ys
          | null xs   = ([],  ys)
          | z == e    = (zs,z:ys)
          | otherwise = answer e zs (z:ys) 
          where
            (z:zs) = xs
