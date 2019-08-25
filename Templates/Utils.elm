module Templates.Utils exposing(getActionButton, getOption, showOptionButton)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick)

import Devs.Objects as O exposing (Month, Model)
import Devs.TypeObject as TO exposing ( .. )

getActionButton: String -> Bool -> Msg -> Html Msg
getActionButton label showBtn event =
  if showBtn then
    Html.input [
      Attr.type_ "button"
      , Attr.class "no-print"
      , Attr.value label
      , Ev.onClick event
    ][]
  else Html.text ""

getOption: String -> String -> String -> Html Msg
getOption key sel label = Html.option [ Attr.value key, Attr.selected (key==sel) ][ Html.text label ]

showOptionButton: O.Model -> Bool
showOptionButton model =
  if model.config.password == Nothing
    then True
    else model.config.loggedIn
