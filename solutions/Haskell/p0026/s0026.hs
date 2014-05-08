import Control.Applicative       ((<$>),(<*>))
import Data.List                 (maximumBy)
import Data.Ord                  (comparing)
import Data.Sequence             (adjust,fromList,index)
import Math.NumberTheory.Primes  (primes)
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

{--
 - Only prime numbers will have longest repeating fractions
 - Note the maximum sequence length for n is n-1
 - Look from upperbound until find n S.T. len(n) = n-1
 - Take the makimum of that list
 -}

type UnitFractionLength = (Int,Int)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args
    in  print . fst $ maxUnitFractionLength limit

getLimit :: [String] -> Int
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 1000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> \n"
         ++ "  Calculates the denominator of the unit fraction \n"
         ++ "  with the longest repeaing decimal expansion  \n"
         ++ "  with the denomination less than <limit>. \n"
         ++ "    <limit>  :: Int (1000)\n"

{-!-}

maxUnitFractionLength :: Int -> UnitFractionLength
maxUnitFractionLength =
  maximumBy (comparing snd) . takePossible .unitFractionLengths
  where
    takePossible xs
      | null xs    = []
      | breakCmp y = [y]
      | otherwise  = y : takePossible ys 
      where
        (y:ys)   = xs
        breakCmp = (==) <$> fst <*> (succ.snd)

unitFractionLengths :: Int -> [UnitFractionLength]
unitFractionLengths lim =
   map getUnitFractionLength . reverse
  . takeWhile (<lim) $ map fromIntegral primes

getUnitFractionLength :: Int -> UnitFractionLength
getUnitFractionLength x = 
  getUnitFractionLength' 1 [] (cleanTally x) x
  where 
    cleanTally = fromList . flip replicate True
    getUnitFractionLength' remainder stack tally denom
      | r == 0           = (denom,0)
      | tally' `index` r = (denom,reps)
      | otherwise        = getUnitFractionLength' r (q:stack) tally' denom 
      where
        (q,r)  = (remainder * 10) `quotRem` denom
        tally' = adjust not r tally
        reps   = length $ takeWhile (/=r) stack
