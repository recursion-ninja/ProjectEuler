module ProjectEuler.Inputs
  ( Input()
  ) where

{-- Fraction doesn't read correctly :( --}

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Data.Ratio          (Rational)

data Input = Decimal  Double
           | Fraction Rational
           | List     [Input]
           | Number   Integer
  deriving (Eq)

instance Show Input where
  show (Decimal  x) = show x
  show (Fraction x) = show x
  show (List     x) = show x
  show (Number    x) = show x

instance Read Input where
  readsPrec n str
    | not $ null int  = (Number   *** id) <$> int
    | not $ null frac = (Fraction *** id) <$> frac
    | not $ null dec  = (Decimal  *** id) <$> dec
    | not $ null list = (List     *** id) <$> list
    | otherwise       = []
    where
      int  = readsPrec n str :: [(Integer , String)]
      frac = readsPrec n str :: [(Rational, String)]
      dec  = readsPrec n str :: [(Double  , String)]
      list = readsPrec n str :: [([Input] , String)]

