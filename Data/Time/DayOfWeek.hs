module Data.Time.DayOfWeek(Gregorian, DayOfWeek(..), 
                           toNextDayOfWeek, 
                           gregorianToNextDayOfWeek,
                           gregorianDayOfWeek,
                           dayOfWeek) 
  where

import Data.Time.Calendar

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq, Show,Enum)

type Gregorian = (Integer, Int, Int) -- see Data.Time.Calendar

dayOfWeek :: Day -> DayOfWeek
dayOfWeek = gregorianDayOfWeek . toGregorian

gregorianDayOfWeek :: Gregorian -> DayOfWeek
gregorianDayOfWeek = toEnum . dayCode

toNextDayOfWeek :: Day -> DayOfWeek -> Day
toNextDayOfWeek date day = addDays (deltaDays (toGregorian date) day) date

gregorianToNextDayOfWeek :: Gregorian -> DayOfWeek -> Gregorian
gregorianToNextDayOfWeek date@(y,m,d) day = 
    toGregorian $ addDays (deltaDays date day) $ fromGregorian y m d

deltaDays :: Gregorian -> DayOfWeek -> Integer
deltaDays date day = 
    let dayCode1 = dayCode date
        dayCode2 = fromEnum day
        tmp = dayCode2 - dayCode1
        days = if tmp < 0 then 7 + tmp else tmp
    in fromIntegral days

dayCode :: Gregorian -> Int
dayCode (year,month,day) = (centuryCode + yearCode + monthCode + day) `mod` 7
  where
    centuryMinus1 = year `div` 100
    leapYear = isLeapYear year

    centuryCode = [0,5,3,1] !! fromIntegral ((centuryMinus1 - 16) `mod` 4)

    yearCode = let y' = fromIntegral $ year - (centuryMinus1*100)
               in y' + (y' `div` 4)

    monthCode = case month of
                  1 | leapYear  -> 5
                    | otherwise -> 6
                  2 | leapYear  -> 1
                    | otherwise -> 2
                  3 -> 2
                  4 -> 5
                  5 -> 0
                  6 -> 3
                  7 -> 5
                  8 -> 1
                  9 -> 4
                  10 -> 6
                  11 -> 2
                  12 -> 4
                  x  -> error $ "invalid month: " ++ show x

