module Templates.Utils exposing(getActionButton, getOption)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick)

import Devs.Objects as O exposing (Month)
import Devs.TypeObject as TO exposing ( .. )

getActionButton: String -> Msg -> Html Msg
getActionButton label event =
  Html.input [
    Attr.type_ "button"
    , Attr.class "no-print"
    , Attr.value label
    , Ev.onClick event
  ][]

getOption: String -> String -> String -> Html Msg
getOption key sel label = Html.option [ Attr.value key, Attr.selected (key==sel) ][ Html.text label ]
