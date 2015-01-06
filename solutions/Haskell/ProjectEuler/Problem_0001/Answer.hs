module ProjectEuler.Problem_0001.Answer
 ( answer
 ) where

import Control.Applicative       ((<$>),(<*>))
import Control.Arrow             ((***))
import Data.List                 (delete)
import Data.Set                  (elems, fromList)
import System.Environment        (getArgs,getProgName)
import Text.Regex                (mkRegex,splitRegex)
import Text.Regex.Base.RegexLike (match)
import qualified ProjectEuler.Problem_0001.Solution as P0001

defaultLimit    = 1000
defaultDivisors = [3,5]

answer :: 
answer = P0001.solution
           defaultLimit
           defaultDivisors
