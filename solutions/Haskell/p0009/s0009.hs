import Control.Applicative ((<$>),(<*>))
import Data.List (sortBy)
import Data.Ord  (comparing)
import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike

{--
 - Notes:
 - Do it correctly!
 - Generate all primative triples with perimeter under limit (use linear algebra)
 - Then generate all triples with perimeter under limit 
 - Then filter based on triples with perimeter exactly equal to limit
 -}

type Tripple        = [Integer]
type TrippleProduct = (Tripple,Integer)

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
  putStr . prettyPrint . map appendProduct . getPythagoreanTriplesOfPerimeter

getPythagoreanTriplesOfPerimeter :: Integer -> [Tripple]
getPythagoreanTriplesOfPerimeter perimeter = 
  filter ((==perimeter).sum) $ getPythagoreanTriplesPerimeterBoundedLimit perimeter

getPythagoreanTriplesPerimeterBoundedLimit :: Integer -> [Tripple]
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

appendProduct :: Tripple -> TrippleProduct
appendProduct = (,) <$> id <*> product

prettyPrint :: [TrippleProduct] -> String
prettyPrint xs
  | null xs   = "None"
  | otherwise = foldl1 (++)
              . map showTripple
              $ sortBy (comparing snd) xs

showTripple :: TrippleProduct -> String
showTripple (x,y) =  show x ++ " : " ++ show y ++ "\n"
