import Data.List                 (maximumBy)
import Data.Ord                  (comparing)
import Data.Sequence             (adjust,fromList,index,Seq)
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex)
import Text.Regex.Base.RegexLike (match)

type FractionExpansion = (Int,([Int],[Int]))

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else
    let limit = getLimit args
    in  putStrLn . showFractionExpansion
      . maximumBy (comparing (length.snd.snd))
      $ fractionExpansions limit

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
         ++ "  Usage: "++name++" <rows> <cols> \n"
         ++ "  Calculates the sum of number of paths which exist \n"
         ++ "  to travel from one corner to the opposite corner  \n"
         ++ "  of a <rows> by <cols> grid. \n"
         ++ "    <rows>  :: Int (20)\n"
         ++ "    <cols>  :: Int (20)\n")

{-!-}

fractionExpansions :: Int -> [FractionExpansion]
fractionExpansions =
  map fractionExpansion . enumFromTo 2
  where
    cleanTally = fromList . flip replicate True
    fractionExpansion :: Int -> FractionExpansion
    fractionExpansion x = fractionExpansion' 1 [] (cleanTally x) x
    fractionExpansion' :: Int -> [Int] -> Seq Bool -> Int -> FractionExpansion
    fractionExpansion' remainder stack tally denom
      | r == 0             = (denom,(stack,[]))
      | (tally' `index` r) = (denom,answer r stack [])
      | otherwise          = fractionExpansion' r (q:stack) tally' denom 
      where
        (q,r)  = (remainder * 10) `quotRem` denom
        tally' = adjust not r tally 
        answer e xs ys
          | null xs   = ([],  ys)
          | z == e    = (zs,z:ys)
          | otherwise = answer e zs (z:ys) 
          where
            (z:zs) = xs

showFractionExpansion :: FractionExpansion -> String
showFractionExpansion (z,(xs,ys)) =
      "1/"
   ++ (show z)
   ++ " = 0."
   ++ (concatMap show xs)
   ++ "("
   ++ (concatMap show ys)
   ++ ")"

