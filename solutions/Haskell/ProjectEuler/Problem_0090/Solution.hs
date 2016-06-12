{-# LANGUAGE TypeFamilies #-}

module ProjectEuler.Problem_0090.Solution -- where
  ( solution
  ) where

import           Data.Foldable
import           Data.List
import           Data.MonoTraversable
import           Data.Vector      (Vector)
import qualified Data.Vector as V
import           Data.Word

newtype Die = Die (Vector Word8)

type instance Element Die = Word8

instance MonoFoldable Die where
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINE ofoldMap #-}

  ofoldr  f e (Die v) = foldr f e v
  {-# INLINE ofoldr #-}

  ofoldl' f e (Die v) = foldl' f e v
  {-# INLINE ofoldl' #-}

  ofoldr1Ex  f (Die v) = foldr1 f v
  {-# INLINE ofoldr1Ex #-}

  ofoldl1Ex' f (Die v) = foldl1 f v
  {-# INLINE ofoldl1Ex' #-}

instance MonoFoldableEq Die where
  x `oelem` (Die v) = x `elem` v

instance Show Die where
  show (Die v) = mconcat [ "{", intercalate "," $ show <$> toList v , "}"]

digits :: [Word8]
digits = [0..9]

dieAssignments :: [Die]
dieAssignments = Die . V.fromList <$> combinations 6 digits

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys
                    | y:xs' <- tails xs
                    , ys    <- combinations (n-1) xs'
                    ]

allSquaresDisplayed :: (Die, Die) -> Bool
allSquaresDisplayed (lhs, rhs) = and
                               [ displays 0 1
                               , displays 0 4
                               , displays 0 9 || displays 0 6
                               , displays 1 6 || displays 1 9
                               , displays 2 5
                               , displays 3 6 || displays 3 9
                               , displays 4 9 || displays 4 6
                               , displays 6 4 || displays 9 4
                               , displays 8 1
                               ]
  where
    displays m n =  (m `oelem` lhs && n `oelem` rhs)
                 || (n `oelem` lhs && m `oelem` rhs)
    
solution :: Int
solution = length $ filter allSquaresDisplayed allAssignments
  where
    allAssignments = (\[x,y] -> (x,y)) <$> combinations 2 dieAssignments
