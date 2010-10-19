
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

gregorianDayOfWeek :: Gregorian -> DayOfWeek
gregorianDayOfWeek = 
    ([Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday] !!) . 
    dayCode

dayOfWeek :: Day -> DayOfWeek
dayOfWeek = gregorianDayOfWeek . toGregorian

dayCode :: Gregorian -> Int
dayCode (y,m,d) = (centuryCode y + yearCode y + monthCode m y + d) `mod` 7

centuryCode :: Integer -> Int
centuryCode x = case ((x`div`100)-16) `mod` 4 of
                  0 -> 0
                  1 -> 5
                  2 -> 3
                  3 -> 1

yearCode :: Integer -> Int
yearCode y = let y' = fromIntegral $ y - ((y `div` 100) * 100)
                 code = y' + floor (fromIntegral y' / 4)
             in (if isLeapYear y then code - 1 else code) `mod` 7

monthCode :: Int -> Integer -> Int
monthCode 1 year | isLeapYear year = 5
                 | otherwise       = 6
monthCode 2 year | isLeapYear year = 1
                 | otherwise       = 2
monthCode 3 _ = 2
monthCode 4 _ = 5
monthCode 5 _ = 0
monthCode 6 _ = 3
monthCode 7 _ = 5
monthCode 8 _ = 1
monthCode 9 _ = 4
monthCode 10 _ = 6
monthCode 11 _ = 2
monthCode 12 _ = 4
monthCode x _ = error $ "invalid month: " ++ show x


