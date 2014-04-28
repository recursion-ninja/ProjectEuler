{--
 - Notes:
 - Do it correctly!
 - Generate all primative triples with perimeter under limit (use linear algebra)
 - Then generate all triples with perimeter under limit 
 - Store only the perimeters of each triple
 - Then group perimeters by equality
 - Then filter based on target grouping size
 - And return quantity of matching matching groups
 -}

import Data.Char
import Data.List
import Data.Ord
import Text.Regex
import Text.Regex.Base.RegexLike
import System.Environment

main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp   name
  else printAnswer args

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <limit> <target>\n"
         ++ "  Calculates the number of perimeters <= <limit>\n"
         ++ "  expressed by exacly <target> pythagorean triples\n"
         ++ "    <limit>  :: Integer (1500000)\n"
         ++ "    <target> :: Int     (1)\n")

printAnswer :: [String] -> IO ()
printAnswer args =
  print . length . correctQuantity target . groupPerimeters . perimetersBoundedByLimit $ limit
  where
    limit  = getLimit  args
    target = getTarget args

getLimit :: [String] -> Integer
getLimit args =
  if   not  $ null args
  then read $ head args 
  else 1500000 --default

getTarget :: [String] -> Int
getTarget args =
  if   length args  > 1
  then read $ args !! 1
  else 1 --default

correctQuantity :: Int -> [[Integer]] -> [[Integer]]
correctQuantity target = 
  filter ((==target).length)

groupPerimeters :: [Integer] -> [[Integer]]
groupPerimeters =
  group . sort

perimetersBoundedByLimit :: Integer -> [Integer]
perimetersBoundedByLimit limit 
  | limit < 12 = [] 
  | otherwise  = concatMap (getPerimetersOfMultiples 1) . map snd . concat $ primatives
  where
    primatives = takeWhile (not.null) $ iterate nextTier [([3,4,5],12)]
    nextTier   = concatMap (filter ((<=limit).snd).triangleMatrixMul)
    -- Branch current primative triple
    -- into three distinct new primative triples
    -- via left multiplication of each matrix
    triangleMatrixMul (t,p) =
      map (\x -> (x, sum x)) $ map (map (sum . zipWith (*) t))
      [[[ 1,-2, 2],
        [ 2,-1, 2],
        [ 2,-2, 3]],
                 [[ 1, 2, 2],
                  [ 2, 1, 2],
                  [ 2, 2, 3]],
                           [[-1, 2, 2],
                            [-2, 1, 2],
                            [-2, 2, 3]]]
    getPerimetersOfMultiples x p
      | newPerimeter <= limit = newPerimeter : getPerimetersOfMultiples (succ x) p
      | otherwise = []
      where newPerimeter = x*p
