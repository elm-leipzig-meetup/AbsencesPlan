module Devs.DatabaseDecode exposing ( dbDecoder )

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Debug exposing (log)

import Devs.Objects as O exposing (..)

dateDecoder : Decoder Date
dateDecoder = Decode.map3 Date
  (field "day" Decode.int)
  (field "month" Decode.int)
  (field "year" Decode.int)

holDecoder : Decoder Holiday
holDecoder = Decode.map4 Holiday
  (field "from" dateDecoder)
  (field "to" dateDecoder)
  (field "holType" Decode.int)
  (field "uuid" Decode.string)

holListDecoder: Decode.Decoder (List Holiday)
holListDecoder = Decode.list holDecoder

configDecoder : Decoder Config
configDecoder = Decode.map7 Config
  (field "maxHoliday" Decode.int)
  (field "maxLearningHoliday" Decode.int)
  (field "holidayURL" Decode.string)
  (field "fedState" Decode.string)
  (field "random" Decode.int)
  (Decode.maybe <| field "password" Decode.string)
  (field "loggedIn" Decode.bool)

dbDecoder : Decoder TransferObj
dbDecoder = Decode.map3 TransferObj
  (field "config" configDecoder)
  (field "holList" holListDecoder)
  (field "init" Decode.bool)
