module HW1.T1
  ( Day (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where
import GHC.Natural

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay day = case day of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays 0 day   = day
afterDays num day = afterDays (num - 1) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
daysToParty day = case day of
  Monday    -> 4
  Tuesday   -> 3
  Wednesday -> 2
  Thursday  -> 1
  Friday    -> 0
  Saturday  -> 6
  Sunday    -> 5