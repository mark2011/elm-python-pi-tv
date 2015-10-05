module Module (appendIf, dayName, dayWithSuffix,
  monthName, nextEntry, shortTime, timeSinceMinute) where

import Date exposing (Date, fromTime, day, dayOfWeek, hour, minute, second, month, year)
import List exposing (map)
import Time

nextEntry : a -> List a -> a -> a
nextEntry default list entry =
  let first = Maybe.withDefault default (List.head list)
      nextEntry' entries = case entries of
        head :: next :: tail -> if head == entry then next else nextEntry' (next :: tail)
        _ -> first
  in nextEntry' list

appendIf b list newTail = list ++ if b then newTail else []

timeSinceMinute date = let t = Date.toTime date in
  t |> Time.inMinutes |> floor |> toFloat |> (*) Time.minute |> (-) t

dayName date = case dayOfWeek date of
   Date.Mon -> "Monday"
   Date.Tue -> "Tuesday"
   Date.Wed -> "Wednesday"
   Date.Thu -> "Thursday"
   Date.Fri -> "Friday"
   Date.Sat -> "Saturday"
   Date.Sun -> "Sunday"

monthName date = case month date of
  Date.Jan -> "January"
  Date.Feb -> "February"
  Date.Mar -> "March"
  Date.Apr -> "April"
  Date.May -> "May"
  Date.Jun -> "June"
  Date.Jul -> "July"
  Date.Aug -> "August"
  Date.Sep -> "September"
  Date.Oct -> "October"
  Date.Nov -> "November"
  Date.Dec -> "December"

dayWithSuffix date =
  let
    dayNumber = day date
    suffix = if
      | dayNumber == 1 || dayNumber == 21 || dayNumber == 31 -> "st"
      | dayNumber == 2 || dayNumber == 22 -> "nd"
      | dayNumber == 3 || dayNumber == 23 -> "rd"
      | otherwise -> "th"
  in
    toString dayNumber ++ suffix

shortTime date =
  let
    hour12 = let h = rem (hour date) 12 in if h == 0 then 12 else h
    minute00 = let m = minute date in (if m < 10 then "0" else "") ++ toString m
    amPm = if hour date < 12 then "AM" else "PM"
  in
    toString hour12 ++ ":" ++ minute00 ++ " " ++ amPm
