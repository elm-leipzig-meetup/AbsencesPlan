module Templates.Forms exposing(getConfigForm, getHolidayForm)

import Html exposing ( .. )
import Html.Attributes as Attr exposing ( .. )
import Html.Events as Ev exposing ( onInput, onFocus )
import Html.Events.Extra as EvE exposing ( onChange )

import Debug exposing (log)

import Devs.Objects as O exposing ( Model )
import Devs.TypeObject as TO exposing ( .. )
import Devs.Utils as DU exposing ( dateToString, dateToDisplaystring, holTypeToString, holTypeToInt, intToHoltype )
import Templates.Utils as TU exposing ( getOption )

--524

getConfigForm: Model -> Html Msg
getConfigForm model =
  if model.showConfigForm
    then getFormDiv model.pageWidth (getConfigRow model) TO.ToggleConfigForm
    else Html.text ""

getHolidayForm: Model -> Html Msg
getHolidayForm model =
  if model.showHolidayForm
    then getFormDiv model.pageWidth (getHolidayRow model) TO.ToggleHolidayForm
    else Html.text ""

getHolidayRow: Model -> Html Msg
getHolidayRow model =
  let
    holFrom = case model.tmpHol of
      Just holiday -> DU.dateToString holiday.from
      Nothing -> ""
    holTo = case model.tmpHol of
      Just holiday -> DU.dateToString holiday.to
      Nothing -> holFrom
    holTypeIdx = case model.tmpHol of
      Just holiday -> holiday.holType
      Nothing -> DU.holTypeToInt O.getDefaultHoliday
    holList = List.filter (\a -> a.from.year == model.currentYear || a.to.year == model.currentYear) (List.sortWith (\a -> \b -> compare (dateToString a.from) (dateToString b.from)) model.holList)
  in
    Html.div[ Attr.class "formDivRow"][
      Html.div [][
        Html.input [ Attr.type_ "date", Attr.id "from", Attr.value holFrom, Ev.onInput TO.SetFrom ][]
        , Html.input [ Attr.type_ "date", Attr.id "to", Attr.value holTo, Ev.onInput TO.SetTo, Ev.onFocus (TO.SetTo holFrom) ][]
        , Html.select [ Attr.id "type", EvE.onChange TO.SetType ]
          (List.map (\i -> TU.getOption (String.fromInt i) (String.fromInt holTypeIdx) (DU.holTypeToString (DU.intToHoltype i))) (List.range 0 3))
        , TU.getActionButton "+" TO.AddHoliday
      ]
      , Html.div [][ Html.table[Attr.class "holTable"](List.map (\h -> Html.tr[][Html.td[][Html.text (DU.holTypeToString (DU.intToHoltype h.holType))], Html.td[][Html.text (DU.dateToDisplaystring h.from)], Html.td[][Html.text (DU.dateToDisplaystring h.to)], Html.td[][TU.getActionButton "-" (TO.RemoveHoliday h.uuid)]]) holList) ]
    ]

getConfigRow: Model -> Html Msg
getConfigRow model =
  Html.div[ Attr.class "formDivRow"][
    Html.div[][
      Html.label [ Attr.for "maxHD" ][ Html.text "Urlaubstage:" ]
      , Html.input [ Attr.type_ "number", Attr.id "maxHD", Attr.value (String.fromInt model.config.maxHoliday), Ev.onInput TO.SetMaxHol ][]
    ]
    , Html.div[][
      Html.label [ Attr.for "maxLHD" ][ Html.text "Bildungsurlaub:" ]
      , Html.input [ Attr.type_ "number", Attr.id "maxLHD", Attr.value (String.fromInt model.config.maxLearningHoliday), Ev.onInput TO.SetLHol ][]
    ]
    , Html.div[][
      Html.label [ Attr.for "fState" ][ Html.text "B-Land:" ]
      , Html.select [ Attr.id "fState", EvE.onChange TO.SetFedState ][
        TU.getOption "BW" model.config.fedState "Baden-Würtemberg"
        , TU.getOption "BY" model.config.fedState "Bayern"
        , TU.getOption "BE" model.config.fedState "Berlin"
        , TU.getOption "BB" model.config.fedState "Brandenburg"
        , TU.getOption "HB" model.config.fedState "Bremen"
        , TU.getOption "HH" model.config.fedState "Hamburg"
        , TU.getOption "HE" model.config.fedState "Hessen"
        , TU.getOption "MV" model.config.fedState "Mecklenburg-Vorpommern"
        , TU.getOption "NI" model.config.fedState "Niedersachsen"
        , TU.getOption "NW" model.config.fedState "Nordrhein-Westfalen"
        , TU.getOption "RP" model.config.fedState "Rheinland-Pfalz"
        , TU.getOption "SL" model.config.fedState "Saarland"
        , TU.getOption "SN" model.config.fedState "Sachsen"
        , TU.getOption "ST" model.config.fedState "Sachsen-Anhalt"
        , TU.getOption "SH" model.config.fedState "Schleswig-Holstein"
        , TU.getOption "TH" model.config.fedState "Thüringen"
      ]
    ]
    , Html.div[][
      Html.label [ Attr.for "url" ][ Html.text "API-URL:" ]
      , Html.input [ Attr.type_ "url", Attr.id "url", Attr.value model.config.holidayURL, Ev.onInput TO.SetUrl ][]
    ]
  ]

getFormDiv: Int -> Html Msg -> Msg -> Html Msg
getFormDiv width subForm event =
  let
    leftSpace = (width - 524) // 2
  in
    Html.div [ Attr.class "formBG" ][
      Html.div [ Attr.class "formDiv", Attr.style "margin-left" ((String.fromInt leftSpace) ++ "px") ] [
        subForm
        , Html.div[ Attr.class "formDivRow"][ TU.getActionButton "schließen" event ]
      ]
    ]
