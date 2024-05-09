module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay day = case day of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

applyN :: Natural -> (Day -> Day) -> Day -> Day
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

afterDays :: Natural -> Day -> Day
afterDays n day = applyN n nextDay day

isWeekend :: Day -> Bool
isWeekend day = case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day    = (+1) $ daysToParty $ (nextDay day)
