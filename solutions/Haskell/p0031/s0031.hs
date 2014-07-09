{--
 - Use Coin Change Algorithm
 --}

--import Debug.Trace   (trace)
import Data.List     (delete,nub,sort)
import Data.Map      (fromList,(!))
import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  if   printHelpParamPassed args
  then printHelp name
  else print $ eulerP0031 (getChange args) (getCurrency args)

getChange :: [String] -> Int
getChange args =
  if   not  $ null args
  then read $ head args
  else 200 --default

getCurrency :: [String] -> [Int]
getCurrency args =
  if   length args > 1
    && not (null list)
  then map read list
  else uk_coinage --default
    where {-- Less fragile then standard read --} 
      list       = parseList $ args !! 1
      parseList  = filter (not . null) . splitRegex (mkRegex ",")
                 . delete '[' . delete ']'
      uk_coinage = [1,2,5,10,20,50,100,200]

printHelpParamPassed :: [String] -> Bool
printHelpParamPassed =
  any (match $ mkRegex "-+[hH](elp)?")

printHelp :: String -> IO ()
printHelp name =
  putStrLn ("\n"
         ++ "  Usage: "++name++" <change> <coins>\n"
         ++ "  Calculates the number of ways the value <change>\n"
         ++ "  can given with the set of currency values <coins>\n"
         ++ "    <change>   :: Int (200)\n"
         ++ "    <coins>    :: CSV Int List (1,2,5,10,20,50,100,200)\n")

{-|-}

eulerP0031 :: Int -> [Int] -> Integer
eulerP0031 = coinChange

coinChange :: Int -> [Int] -> Integer
coinChange n s = count ! (n,m)
  where
    m     = length s'
    s'    = sort $ nub s                       -- Coin values must be in ascending order
    s''   = fromList $ zip (enumFromTo 1 m) s' -- Use Map for better access (1 indexed)
    count = fromList [ ( (i,j), logic i j) |
                       i <- [negate (maximum s') .. n],
                       j <- [0..m]
                     ]
    {-- 
     - Number of ways to give N with lowest m coins is:
     - Number of ways to give N with lowest m-1 coins
     - Plus ways to give N - m's coin value, with lowest m coins
     -}
    logic n' m'
      | n' == 0   = 1 -- Can     make change evenly this way
      | n' <  0   = 0 -- Can NOT make change evenly this way
      | m' <= 0   = 0 -- Requires a non-existing coin
      | otherwise =  count ! (n', m'-1)
                  + (count ! (n' - (s'' ! m'), m'))
