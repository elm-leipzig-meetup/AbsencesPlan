module Devs.Utils exposing (getSeed, updatePublicHolidayListInConfig, getPubHolListOfYear, setPublicHolidays, fillYear, dateToString, dateToDisplaystring, stringToDate, holTypeToString, intToHoltype, holTypeToInt, isDayForCount, getDaysForSummery)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (..)
import Task
import Http
import Random
import Time as T exposing (Zone,Month)
import List.Extra as ListE exposing ( .. )
import Derberos.Date.Core as DerC
import Derberos.Date.Calendar as DerCal
import Derberos.Date.Utils as DerU
import Derberos.Date.Delta as DerD

import Debug exposing (log)

import Devs.Objects as O exposing (Model, PublicHoliday, Year, Month)
import Devs.TypeObject as TO exposing ( Msg )
import Devs.Decode as Dec exposing ( holidayDecoder )

getSeed: Model -> Random.Seed
getSeed model =
  case model.currentSeed of
      Just seed ->  seed
      Nothing -> Random.initialSeed model.config.random

getDaysForSummery: O.HolType -> O.Year -> Int
getDaysForSummery holType yearCal = List.sum (List.map (\m -> List.length (List.filter (\d -> isDayForCount d holType) m.days)) yearCal.months)

updatePublicHolidayListInConfig: O.Config -> O.PubHolYear -> O.Config
updatePublicHolidayListInConfig config pubHolYear =
  if List.length (List.filter (\item -> item.year == pubHolYear.year) config.pubHolList) > 0
    then config
    else { config | pubHolList = List.append config.pubHolList [pubHolYear] }

fillYear: Int -> T.Zone -> List O.PublicHoliday -> List O.Holiday -> O.Year
fillYear year zone pHolidays holidays =
  let
    z = T.utc
    fDoY = DerC.newDateRecord year 1 1 1 0 0 0 z
    fDoYP = DerC.civilToPosix fDoY
    months = getMonths fDoY z pHolidays holidays
  in
    { name=year, months=months }

getMonths: DerC.DateRecord -> T.Zone -> List O.PublicHoliday -> List O.Holiday -> List O.Month
getMonths fDoY zone pHolidays holidays = List.map (getMonth zone fDoY pHolidays holidays) (List.range 0 11)

getMonth: T.Zone -> DerC.DateRecord -> List O.PublicHoliday -> List O.Holiday -> Int -> O.Month
getMonth zone fDoY pHolidays holidays monthNr =
  let
    z = zone
    d = (DerC.newDateRecord fDoY.year (monthNr + 1) 3 15 0 0 0 z)
    date = (DerC.civilToPosix d)
    monthName = case DerU.numberToMonth monthNr of
      Just m -> toGermanMonth m
      Nothing -> "noMonth"
    dayList = (DerCal.getCurrentMonthDates z date)
    days = List.map (posixToDay pHolidays holidays z) dayList
    weekList = (DerCal.getCurrentMonthDatesFullWeeks z date)
    weekDays = List.map (posixToWeekday pHolidays holidays z) weekList
    weeks = List.map (getWeek fDoY z) (ListE.groupsOf 7 weekDays)
  in
    { name = monthName, nr=monthNr, days = days, weeks = weeks }

getWeek: DerC.DateRecord -> T.Zone -> List O.WeekDay -> O.Week
getWeek fDoY zone weekDays =
  let
    fWDoW = case List.head weekDays of
      Just wd -> wd
      Nothing -> {name="", day={holidayName=Nothing, holType=Nothing, date={ day = fDoY.day, month = fDoY.month, year = fDoY.year }}}
    date1 = case List.head (DerCal.getCurrentWeekDates zone (DerC.civilToPosix fDoY)) of
      Just date -> T.posixToMillis date
      Nothing -> 0
    date2 = T.posixToMillis (DerC.civilToPosix (dateToCivil zone fWDoW.day.date))
    diff = (getDiffOfTS date1 date2) // 7
  in
    { nr=(diff + 1), weekDays=weekDays }

getDiffOfTS: Int -> Int -> Int
getDiffOfTS date1 date2 = (((((date2 - date1) // 1000) // 60) // 60) // 24)

posixToWeekday: List O.PublicHoliday -> List O.Holiday -> T.Zone -> T.Posix -> O.WeekDay
posixToWeekday pHolidays holidays zone day =
  let
    d = posixToDay pHolidays holidays zone day
    dName = toGermanWeekday (DerU.getWeekday zone day)
  in
    { name=dName, day=d }

posixToDay: List O.PublicHoliday -> List O.Holiday -> T.Zone -> T.Posix -> O.Day
posixToDay pHolidays holidays zone day =
  let
    d = DerC.posixToCivil day
    phd = case ListE.find ( \a -> a.date==(daterecordToString d) ) pHolidays of
      Just ph -> Just ph.title
      Nothing -> Nothing
    hd = getHoldayTypeOfDate d holidays zone
    --_ = Debug.log "d: " d
    date = { day = d.day, month = d.month, year = d.year }
    ht = case hd of
      Just h -> Just (holTypeToInt h)
      Nothing -> Nothing
  in
    { date = date, holidayName = phd, holType=ht }

getHoldayTypeOfDate: DerC.DateRecord -> List O.Holiday -> T.Zone -> Maybe O.HolType
getHoldayTypeOfDate date hList zone =
  let
    holList = List.filter (\a -> a.from.year == date.year || a.to.year == date.year) hList
    holTypeList = List.filter (\a -> a /= Nothing) (List.map (getHoltypeFromDate date zone) holList)
  in
    Maybe.withDefault (Nothing) (List.head holTypeList)

getHoltypeFromDate: DerC.DateRecord -> T.Zone -> O.Holiday -> Maybe O.HolType
getHoltypeFromDate date zone hol =
  let
    posix1 = DerC.civilToPosix (dateToCivil zone hol.from)
    posix2 = DerC.civilToPosix (dateToCivil zone hol.to)
    diff = (getDiffOfTS (T.posixToMillis posix1) (T.posixToMillis posix2))
    holList = List.map (\i -> civilToDate (DerC.posixToCivil (DerD.addDays i posix1))) (List.range 0 diff)
  in
    case ListE.find (\a -> a.day==date.day && a.month==date.month && a.year==date.year) holList of
      Just h -> Just (intToHoltype hol.holType)
      Nothing -> Nothing

civilToDate: DerC.DateRecord -> O.Date
civilToDate d = {day=d.day, month=d.month, year=d.year}

dateToCivil: T.Zone -> O.Date -> DerC.DateRecord
dateToCivil zone d = DerC.newDateRecord d.year d.month d.day 15 0 0 0 zone

daterecordToString: DerC.DateRecord -> String
daterecordToString d = ((String.fromInt d.year) ++ "-" ++ (String.padLeft 2 '0' (String.fromInt d.month)) ++ "-" ++ (String.padLeft 2 '0' (String.fromInt d.day)))

dateToString: O.Date -> String
dateToString date = daterecordToString (dateToCivil T.utc date)

dateToDisplaystring: O.Date -> String
dateToDisplaystring d = ((String.padLeft 2 '0' (String.fromInt d.day)) ++ "." ++ (String.padLeft 2 '0' (String.fromInt d.month)) ++ "." ++ (String.fromInt d.year))

stringToDate: String -> O.Date
stringToDate str =
  let
    strArr = String.split "-" str
    year = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListE.getAt 0 strArr)))
    month = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListE.getAt 1 strArr)))
    day = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListE.getAt 2 strArr)))
  in
    {day=day, month=month, year=year}

holTypeToString: O.HolType -> String
holTypeToString hType = case hType of
  O.Ill -> "Krank"
  O.Kar -> "Karenz"
  O.Hol -> "Urlaub"
  O.LHol -> "Bildungsurlaub"

intToHoltype: Int -> O.HolType
intToHoltype idx = if idx==0 then O.Ill
  else if idx==1 then O.Kar
  else if idx==2 then O.Hol
  else if idx==3 then O.LHol
  else O.getDefaultHoliday

holTypeToInt: O.HolType -> Int
holTypeToInt hType = case hType of
  O.Ill -> 0
  O.Kar -> 1
  O.Hol -> 2
  O.LHol -> 3

isDayForCount: O.Day -> O.HolType -> Bool
isDayForCount d ht =
  let
    zone = T.utc
    date = (DerC.civilToPosix (dateToCivil zone d.date))
    wd = DerU.getWeekday zone date
  in
    d.holType == Just (holTypeToInt ht) && d.holidayName == Nothing && (wd /= T.Sat && wd /= T.Sun)

toGermanWeekday : T.Weekday -> String
toGermanWeekday wd =
  case wd of
    T.Mon -> "Mo"
    T.Tue -> "Di"
    T.Wed -> "Mi"
    T.Thu -> "Do"
    T.Fri -> "Fr"
    T.Sat -> "Sa"
    T.Sun -> "So"
{-
    T.Mon -> "Montag"
    T.Tue -> "Dienstag"
    T.Wed -> "Mittwoch"
    T.Thu -> "Donnerstag"
    T.Fri -> "Freitag"
    T.Sat -> "Samstag"
    T.Sun -> "Sonntag"
-}

toGermanMonth : T.Month -> String
toGermanMonth month =
  case month of
    T.Jan -> "Januar"
    T.Feb -> "Februar"
    T.Mar -> "März"
    T.May -> "Mai"
    T.Apr -> "April"
    T.Jun -> "Juni"
    T.Jul -> "Juli"
    T.Aug -> "August"
    T.Sep -> "September"
    T.Oct -> "Oktober"
    T.Nov -> "November"
    T.Dec -> "Dezember"

setPublicHolidays: Model -> Cmd Msg
setPublicHolidays model =
  let
    url1 = String.replace "[year]" (String.fromInt model.currentYear) model.config.holidayURL
    url2 = String.replace "[fedState]" model.config.fedState url1
  in
    setPublicHolidaysApi TO.SetPublicHolidays url2

getPubHolListOfYear: List O.PubHolYear -> Int -> String -> List O.PublicHoliday
getPubHolListOfYear pubHolYearList currentYear fedState =
  case List.head (List.filter (\phy -> phy.year == currentYear && phy.fedState == fedState) pubHolYearList) of
    Just phy -> phy.pubHolList
    Nothing -> []

setPublicHolidaysApi: (Result Http.Error (List O.PublicHoliday) -> Msg) -> String -> Cmd Msg
setPublicHolidaysApi event url = myRequest "GET" url (Http.expectJson event (Decode.list Dec.holidayDecoder)) Nothing

myRequest: String -> String -> (Http.Expect Msg) -> Maybe Value -> Cmd Msg
myRequest method url expect content =
  let
      header = []
      body = case content of
        Just reqBody -> Http.jsonBody reqBody
        Nothing -> Http.emptyBody
  in
     Http.request -- This line is missing from your code
      { method = method
      , headers = header
      , url = url
      , body = body
      , expect = expect
      , timeout = Nothing
      , tracker = Nothing
      }
