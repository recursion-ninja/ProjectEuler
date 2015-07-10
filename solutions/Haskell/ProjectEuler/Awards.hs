module ProjectEuler.Awards
  ( Award(..)
  ) where

import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Data.Ratio
import Data.List ((\\),intersect,partition)
import ProjectEuler (ProblemNumber,answer)

data Award = BabySteps | TheJourneyBegins | UnluckySquares
  deriving (Bounded,Eq,Enum,Read,Show)

data AwardProgress = AwardProgress Award Progress

data Progress = Completed | Progress Int Int
  deriving (Read)

instance Show Progress where
  show Completed      = "Completed"
  show (Progress x y) = show x ++ "/" ++ show y

instance Show AwardProgress where
  show (AwardProgress award progress) = show award ++ ": " ++ show progress


applicableProblems :: Award -> [ProblemNumber]
applicableProblems BabySteps        = allProblems
applicableProblems TheJourneyBegins = allProblems
applicableProblems UnluckySquares   = (toEnum <$>)
                                    $ ((^2) <$> allNumbers) 
                                   `intersect ` allNumbers

allProblems :: [ProblemNumber]
allProblems = [minBound..maxBound] 

allNumbers :: [Int]
allNumbers = fromEnum <$> allProblems 

awardThreshhold :: Award -> Int
awardThreshhold BabySteps        = 3
awardThreshhold TheJourneyBegins = 25
awardThreshhold UnluckySquares   = 13

awardPartition :: Award -> ([ProblemNumber], [ProblemNumber])
awardPartition = partition (isJust . answer) . applicableProblems

getProgress :: Award -> Progress
getProgress x
  | count >= threshhold = Completed
  | otherwise = Progress count threshhold 
  where
    threshhold    = awardThreshhold x
    count         = length completed
    (completed,_) = awardPartition x

awardProgress :: Award -> AwardProgress
awardProgress = AwardProgress <$> id <*> getProgress
