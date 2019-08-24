port module HolidayPlan exposing (..)

import Devs.Ports as P exposing (setDataFromStore)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing ( .. )

import Devs.Objects as O exposing (Model, initialModel)
import Devs.Update as U exposing ( update )
import Devs.Utils as DU exposing ( setPublicHolidays )
import Devs.TypeObject as TO exposing ( .. )
import Templates.Month as TM exposing ( getMonth, getSummeryDiv )
import Templates.Utils as TU exposing ( getActionButton )
import Templates.Forms as TF exposing ( getConfigForm, getHolidayForm )

import Debug exposing (log)

-- Methods

-- View
view : O.Model -> Html TO.Msg
view model =
    Html.div [] (
      List.concat [
        [ TF.getConfigForm model
          , TF.getHolidayForm model
          , Html.h1 [][
            Html.text "Kalender des Jahres "
            , TU.getActionButton "<" (TO.ShiftYear O.Down)
            , Html.text (" " ++ (String.fromInt model.calendar.name) ++ " ")
            , TU.getActionButton ">" (TO.ShiftYear O.Up)
          ]
          , Html.div [][
            Html.span [ Attr.style "margin-left" "5pt" ][]
            ,TU.getActionButton "Konfiguration" (TO.ToggleConfigForm)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Abwesenheiten" (TO.ToggleHolidayForm)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Export" (TO.DownloadDB)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Import" (TO.ImportDB)
          ]
        ]
        ,  ( List.map TM.getMonth model.calendar.months )
        , [ TM.getSummeryDiv model ]
      ]
    )

main : Program () O.Model TO.Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = U.update
        , subscriptions = subscriptions
        }

subscriptions : O.Model -> Sub TO.Msg
subscriptions model = P.setDataFromStore TO.ReadDataFromPublish

init : ( O.Model, Cmd Msg )
init =  ( O.initialModel, P.pushDataToStore {config=O.initialModel.config, holList=O.initialModel.holList, init=True} )
