module Templates.Forms exposing(getConfigForm, getHolidayForm, geLoginForm)

import Html exposing ( .. )
import Html.Attributes as Attr exposing ( .. )
import Html.Events as Ev exposing ( onInput, onFocus, onBlur )
import Html.Events.Extra as EvE exposing ( onChange )

import Debug exposing (log)

import Devs.Objects as O exposing ( Model )
import Devs.TypeObject as TO exposing ( .. )
import Devs.Utils as DU exposing ( dateToString, dateToDisplaystring, holTypeToString, holTypeToInt, intToHoltype )
import Templates.Utils as TU exposing ( getOption )

--524

geLoginForm: Model -> Html Msg
geLoginForm model =
  if model.showLoginForm
    then getFormDiv model.pageWidth (getLoginRow model) TO.ToggleLoginForm
    else Html.text ""

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

getLoginRow: Model -> Html Msg
getLoginRow model =
    Html.div[ Attr.class "formDivRow"][
      Html.div [][
        Html.input [ Attr.type_ "password", Attr.id "pw", Ev.onInput TO.SetPW ][]
      ]
    ]

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
        , TU.getActionButton "+" True TO.AddHoliday
      ]
      , Html.div [][ Html.table[Attr.class "holTable"](List.map (\h -> Html.tr[][Html.td[][Html.text (DU.holTypeToString (DU.intToHoltype h.holType))], Html.td[][Html.text (DU.dateToDisplaystring h.from)], Html.td[][Html.text (DU.dateToDisplaystring h.to)], Html.td[][TU.getActionButton "-" True (TO.RemoveHoliday h.uuid)]]) holList) ]
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
        Html.label [ Attr.for "pw" ][ Html.text "Passwort:" ]
        , Html.input [ Attr.type_ "text", Attr.id "pw", Attr.value (Maybe.withDefault "" model.tmpPw), Ev.onInput TO.SetPW ][]
      ]
      , Html.div[][
        Html.label [ Attr.for "fState" ][ Html.text "B-Land:" ]
        , Html.select [ Attr.id "fState", EvE.onChange TO.SetFedState ]
        (List.map (getOption model) [("BY", "Bayern")
          ,("BE", "Berlin")
          ,("BB", "Brandenburg")
          ,("HB", "Bremen")
          ,("HH", "Hamburg")
          ,("HE", "Hessen")
          ,("MV", "Mecklenburg-Vorpommern")
          ,("NI", "Niedersachsen")
          ,("NW", "Nordrhein-Westfalen")
          ,("RP", "Rheinland-Pfalz")
          ,("SL", "Saarland")
          ,("SN", "Sachsen")
          ,("ST", "Sachsen-Anhalt")
          ,("SH", "Schleswig-Holstein")
          ,("TH", "Thüringen")]
        )
      ]
      , Html.div[][
        Html.label [ Attr.for "url" ][ Html.text "API-URL:" ]
        , Html.input [ Attr.type_ "url", Attr.id "url", Attr.value model.config.holidayURL, Ev.onInput TO.SetUrl ][]
      ]
    ]

getOption: Model -> (String, String) -> Html Msg
getOption model (key, value) = TU.getOption key model.config.fedState value

getFormDiv: Int -> Html Msg -> Msg -> Html Msg
getFormDiv width subForm event =
  let
    leftSpace = (width - 524) // 2
  in
    Html.div [ Attr.class "formBG" ][
      Html.div [ Attr.class "formDiv", Attr.style "margin-left" ((String.fromInt leftSpace) ++ "px") ] [
        subForm
        , Html.div[ Attr.class "formDivRow"][ TU.getActionButton "schließen" True event ]
      ]
    ]
