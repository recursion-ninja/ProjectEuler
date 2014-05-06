{--
 - Notes:
 - Do it correctly!
 - Generate all primative triples with perimeter under limit (use linear algebra)
 - Then generate all triples with perimeter under limit 
 - Then filter based on triples with perimeter exactly equal to limit
 -}

import System.Environment
import Data.List
import Text.Regex
import Text.Regex.Base.RegexLike

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else printAnswer $ getTarget args

getTarget :: [String] -> Integer
getTarget args =
  if   not  $ null args
  then read $ head args 
  else 1000 --default

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <target>\n"
         ++ "  Calculates the product of the sides of all\n"
         ++ "  pythagorean triples with a perimeter of <target>\n"
         ++ "    <target> :: Integer (1000)\n")

{-!-}

printAnswer :: Integer -> IO ()
printAnswer =
  putStr . prettyPrint . appendProduct . getPythagoreanTriplesOfPerimeter

getPythagoreanTriplesOfPerimeter :: Integer -> [[Integer]]
getPythagoreanTriplesOfPerimeter perimeter = 
  filter ((==perimeter).sum) $ getPythagoreanTriplesPerimeterBoundedLimit perimeter

getPythagoreanTriplesPerimeterBoundedLimit :: Integer -> [[Integer]]
getPythagoreanTriplesPerimeterBoundedLimit perimeter 
  | perimeter < 12 = [] 
  | otherwise      = concatMap (tMultiples 1) $ concat primatives
  where
    primatives     = takeWhile (not.null) $ iterate nextTier [[3,4,5]]
    nextTier       = concatMap (filter ((<=perimeter).sum).tMatrixMul)
    tMatrixMul t   = map (map (sum . zipWith (*) t))
      [[[ 1,-2,2],
        [ 2,-1,2],
        [ 2,-2,3]],
       [[ 1, 2,2],
        [ 2, 1,2],
        [ 2, 2,3]],
       [[-1, 2,2],
        [-2, 1,2],
        [-2, 2,3]]]
    tMultiples x t = 
      let newTriangle = map (*x) t
      in
        if perimeter >= sum newTriangle
        then newTriangle : tMultiples (x+1) t
        else []

appendProduct :: [[Integer]] -> [([Integer],Integer)]
appendProduct = 
  map (\x -> (x, product x))

prettyPrint :: [([Integer],Integer)] -> String
prettyPrint list = 
  foldl (\x (y,z) -> x ++  show y ++" : "++ show z ++"\n") ""
  $ sortBy (\(_,x) (_,z) -> compare z x) list
