port module HolidayPlan exposing (..)

import Devs.Ports as P exposing (setDataFromStore, setDimOfElement)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing ( .. )
import Html.Events as Ev exposing ( on )

import Json.Decode as Json

import Devs.Objects as O exposing (Model, initialModel)
import Devs.Update as U exposing ( update )
import Devs.Utils as DU exposing ( setPublicHolidays )
import Devs.TypeObject as TO exposing ( .. )
import Templates.Month as TM exposing ( getMonth, getSummeryDiv )
import Templates.Utils as TU exposing ( getActionButton, showOptionButton )
import Templates.Forms as TF exposing ( getConfigForm, getHolidayForm,geLoginForm )

import Debug exposing (log)

-- Methods

-- View
view : O.Model -> Html TO.Msg
view model =
    Html.div [ ] (
      List.concat [
        [ TF.getConfigForm model
          , TF.getHolidayForm model
          , TF.geLoginForm model
          , Html.h1 [][
            Html.text "Kalender des Jahres "
            , TU.getActionButton "<" True (TO.ShiftYear O.Down)
            , Html.text (" " ++ (String.fromInt model.calendar.name) ++ " ")
            , TU.getActionButton ">" True (TO.ShiftYear O.Up)
          ]
          , Html.div [][
            Html.span [ Attr.style "margin-left" "5pt" ][]
            ,TU.getActionButton "Konfiguration" (TU.showOptionButton model) (TO.ToggleConfigForm)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Abwesenheiten" (TU.showOptionButton model) (TO.ToggleHolidayForm)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Export" (TU.showOptionButton model) (TO.DownloadDB)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Import" True (TO.ImportDB)
            , Html.span [ Attr.style "margin-left" "5pt" ][]
            , TU.getActionButton "Login" (model.config.password /= Nothing && not model.config.loggedIn) (TO.ToggleLoginForm)
            , TU.getActionButton "Logout" (model.config.password /= Nothing && model.config.loggedIn) (TO.Logout)
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
subscriptions model = Sub.batch [
    P.setDataFromStore TO.ReadDataFromPublish
    , P.setDimOfElement TO.SetDomDim
  ]

init : ( O.Model, Cmd Msg )
init =  ( O.initialModel, Cmd.batch [
      P.pushDataToStore {config=O.initialModel.config, holList=O.initialModel.holList, init=True}
      , P.getDimOfElement "app"
    ]
  )
