module RikardCorp.Helpers.DateTime where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Printf
import Data.Time.Format

subtractYearsFromCurrent :: Integer -> IO (UTCTime, UTCTime)
subtractYearsFromCurrent years = do
  UTCTime day1 tm1 <- getCurrentTime
  let day2 = addDays ((-365) * years) day1
  return (UTCTime day2 tm1, UTCTime day1 tm1)

subtractDaysFromCurrent :: Integer -> IO (UTCTime, UTCTime)
subtractDaysFromCurrent days = do
  UTCTime day1 tm1 <- getCurrentTime
  let day2 = addDays ((-1) * days) day1
  return (UTCTime day2 tm1, UTCTime day1 tm1)

formatUTCToCurrentLocTime :: UTCTime -> IO String
formatUTCToCurrentLocTime utc' = do
  cTZ <- getCurrentTimeZone

  let
    lcTime :: LocalTime
    lcTime = utcToLocalTime cTZ utc'

    fmttm :: LocalTime -> String
    fmttm = formatTime defaultTimeLocale "%Y%m%dT%H%M"

  return $ fmttm lcTime

getCurrentUTCDay :: IO UTCTime
getCurrentUTCDay = do
   UTCTime day _ <- getCurrentTime
   return $ UTCTime day 0

getCurrentYear :: IO Integer
getCurrentYear = do
  cUTC <- getCurrentTime
  cTZ <- getCurrentTimeZone

  let
    lcTime :: LocalTime
    lcTime = utcToLocalTime cTZ cUTC

    lcDay :: Day
    lcDay = localDay lcTime

    (year, _, _) = toGregorian lcDay

  return year

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

dmyToUTC :: String -> Maybe UTCTime
dmyToUTC = parseTimeM True defaultTimeLocale "%d.%m.%Y"

uniDateToUTC :: String -> Maybe UTCTime
uniDateToUTC = parseTimeM True defaultTimeLocale "%Y-%m-%d"

uniDateTimeToUTC :: String -> Maybe UTCTime
uniDateTimeToUTC = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

dateString :: IO String
dateString = do
  ct <- getCurrentTime
  cz <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale
    "%d.%m.%Y" $ utcToLocalTime cz ct

isCurrentDay :: UTCTime -> IO Bool
isCurrentDay tm = do
  ct <- getCurrentTime

  let
    lcDay :: Day
    lcDay = localDay (utcToLocalTime utc tm)

    lcDay' :: Day
    lcDay' = localDay (utcToLocalTime utc ct)

  return (lcDay == lcDay')

fmtTimeToHumanDay :: UTCTime -> String
fmtTimeToHumanDay tm =
  let
    lcTime :: LocalTime
    lcTime = utcToLocalTime utc tm

    lcDay :: Day
    lcDay = localDay lcTime

    (year, month, day) = toGregorian lcDay
  in
    printf "%d %s %d г." day (humanMonth month) year

humanMonth :: Int -> String
humanMonth 1  = "января"
humanMonth 2  = "февраля"
humanMonth 3  = "марта"
humanMonth 4  = "апреля"
humanMonth 5  = "мая"
humanMonth 6  = "июня"
humanMonth 7  = "июля"
humanMonth 8  = "августа"
humanMonth 9  = "сентября"
humanMonth 10 = "октября"
humanMonth 11 = "ноября"
humanMonth 12 = "декабря"
humanMonth _  = "undef"
