module Templates.Month exposing(getMonth, getSummeryDiv)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

import Devs.Objects as O exposing (Model, Month)
import Devs.TypeObject as TO exposing ( .. )
import Devs.Utils as DU exposing ( holTypeToString, isDayForCount )

getMonth: O.Month -> Html Msg
getMonth month =
  let
    week = case List.head month.weeks of
      Just w -> w
      Nothing -> { nr=0, weekDays=[] }
  in
    Html.div[ Attr.class "monthDiv" ][
      Html.table [][
        Html.thead [][
          Html.tr [][ Html.th [ Attr.colspan 8 ][ Html.text month.name ] ]
          , Html.tr []( List.append [ Html.th [][ Html.text "Nr." ] ] ( List.map getWeekdayHeader week.weekDays ) )
        ]
        , Html.tbody []( List.map (getWeek month.nr) month.weeks )
      ]
    ]

getWeekdayHeader: O.WeekDay -> Html Msg
getWeekdayHeader wd = Html.th [][ Html.text wd.name ]

getWeek: Int -> O.Week -> Html Msg
getWeek mNr week =
  Html.tr [](
    List.append [ Html.th [][ Html.text ( String.fromInt week.nr ) ] ]
      ( List.map (getWeekday mNr) week.weekDays )
  )

getWeekday: Int ->  O.WeekDay -> Html Msg
getWeekday mNr wd =
  let
    dayClass = if (mNr + 1) /= wd.day.date.month then "otherMonth"
      else if wd.name == "Sa" || wd.name == "So" then "weekend"
      else if wd.day.holidayName /= Nothing then "publicHoliday"
      else if wd.day.holType == Just (DU.holTypeToInt O.LHol) || wd.day.holType == Just (DU.holTypeToInt O.Hol) then "holiday"
      else if wd.day.holType == Just (DU.holTypeToInt O.Ill) || wd.day.holType == Just (DU.holTypeToInt O.Kar) then "illness"
      else ""
    holidayName = case wd.day.holidayName of
        Just name -> name
        Nothing -> case wd.day.holType of
          Just holType -> DU.holTypeToString (DU.intToHoltype holType)
          Nothing -> ""
  in
    Html.td [ Attr.class dayClass, Attr.title holidayName ][ Html.text (String.padLeft 2 '0' (String.fromInt wd.day.date.day)) ]

getSummeryDiv: O.Model -> Html Msg
getSummeryDiv model =
  let
    sumHol = List.sum (List.map (\m -> List.length (List.filter (\d -> DU.isDayForCount d O.Hol) m.days)) model.calendar.months)
    sumLhol = List.sum (List.map (\m -> List.length (List.filter (\d -> DU.isDayForCount d O.LHol) m.days)) model.calendar.months)
    sumIll = List.sum (List.map (\m -> List.length (List.filter (\d -> DU.isDayForCount d O.Ill) m.days)) model.calendar.months)
    sumKar = List.sum (List.map (\m -> List.length (List.filter (\d -> DU.isDayForCount d O.Kar) m.days)) model.calendar.months)
    lHolRow = if sumLhol > 0
      then Html.tr [][
          Html.th [][ Html.text (DU.holTypeToString (DU.intToHoltype 3)) ]
          ,Html.td [][ Html.text (String.fromInt model.config.maxLearningHoliday) ]
          ,Html.td [][ Html.text (String.fromInt sumLhol) ]
          ,Html.td [][ Html.text (String.fromInt (model.config.maxLearningHoliday - sumLhol)) ]
        ]
      else Html.text ""
  in
    Html.div [ Attr.class "summeryDiv" ][
      Html.table [][
        Html.tr [][
          Html.th [][ Html.text "" ]
          , Html.th [][ Html.text "Soll" ]
          , Html.th [][ Html.text "Ist" ]
          , Html.th [][ Html.text "Rest" ]
        ]
        , Html.tr [][
          Html.th [][ Html.text (DU.holTypeToString (DU.intToHoltype 2)) ]
          ,Html.td [][ Html.text (String.fromInt model.config.maxHoliday) ]
          ,Html.td [][ Html.text (String.fromInt sumHol) ]
          ,Html.td [][ Html.text (String.fromInt (model.config.maxHoliday - sumHol)) ]
        ]
        , lHolRow
        , Html.tr [][
          Html.th [][ Html.text (DU.holTypeToString (DU.intToHoltype 0)) ]
          ,Html.td [][ Html.text "" ]
          ,Html.td [][ Html.text (String.fromInt sumIll) ]
          ,Html.td [][ Html.text "" ]
        ]
        , Html.tr [][
          Html.th [][ Html.text (DU.holTypeToString (DU.intToHoltype 1)) ]
          ,Html.td [][ Html.text "" ]
          ,Html.td [][ Html.text (String.fromInt sumKar) ]
          ,Html.td [][ Html.text "" ]
        ]
      ]
    ]
