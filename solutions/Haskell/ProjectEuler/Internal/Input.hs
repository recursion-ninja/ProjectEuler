module ProjectEuler.Internal.Input
  ( (~%~)
  , DefaultInput()
  , Input(..)
  , defaultValue
  , tooltip
  ) where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
--import Data.Ratio          (Rational)

data DefaultInput 
   = DefaultInput
   { tooltip      :: String
   , defaultValue :: Input
   }
   deriving (Eq)

(~%~) :: String -> Input -> DefaultInput
(~%~) = DefaultInput

instance Show DefaultInput where
  show x = unwords
         [ tooltip x 
         , ":: " ++ (tipe . defaultValue) x
         , "("   ++ (show . defaultValue) x ++ ")"
         ]

data Input 
   = D Double
   | F Rational
   | L [Input]
   | I Integer
   deriving (Eq)

instance Show Input where
  show (D x) = show x
  show (F x) = show x
  show (L x) = show x
  show (I x) = show x

{-- Fraction doesn't read correctly --}
instance Read Input where
  readsPrec n str
    | not $ null int  = (I *** id) <$> int
    | not $ null frac = (F *** id) <$> frac
    | not $ null dec  = (D *** id) <$> dec
    | not $ null list = (L *** id) <$> list
    | otherwise       = []
    where
      int  = readsPrec n str :: [(Integer , String)]
      frac = readsPrec n str :: [(Rational, String)]
      dec  = readsPrec n str :: [(Double  , String)]
      list = readsPrec n str :: [([Input] , String)]

tipe :: Input -> String
tipe (D  _) = "Double"
tipe (F  _) = "Rational"
tipe (L xs) = "[" ++ (tipe . head) xs ++ "]"
tipe (I  _) = "Integer"
