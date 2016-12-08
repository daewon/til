import Data.Time
import Data.Time.Calendar.WeekDate

main = do
    now <- getCurrentTime
    print now

    let next = UTCTime { utctDay = fromGregorian 2009 11 17
                       , utctDayTime = timeOfDayToTime (TimeOfDay 20 34 58.651387237)
                       }
    print next

    let (year, month, day) = toGregorian . utctDay $ next
    print year
    print month
    print day
    let hour = todHour . timeToTimeOfDay . utctDayTime $ next
    let minute = todMin . timeToTimeOfDay . utctDayTime $ next
    let (second, nano) = properFraction . todSec . timeToTimeOfDay . utctDayTime $ next
    let (_, _, week) = toWeekDate . utctDay $ next
    print hour
    print minute
    print second
    print nano
    print week

    print $ next < now
    print $ next > now
    print $ next == now
