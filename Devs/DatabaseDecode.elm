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

pubHolDecoder: Decoder PublicHoliday
pubHolDecoder = Decode.map2 PublicHoliday
  (field "title" Decode.string)
  (field "date" Decode.string)

pubHolListDecoder: Decode.Decoder (List PublicHoliday)
pubHolListDecoder = Decode.list pubHolDecoder

pubHolYearDecoder: Decoder PubHolYear
pubHolYearDecoder = Decode.map3 PubHolYear
  (field "year" Decode.int)
  (field "fedState" Decode.string)
  (field "pubHolList" pubHolListDecoder)

pubHolYearListDecoder: Decode.Decoder (List PubHolYear)
pubHolYearListDecoder = Decode.list pubHolYearDecoder

configDecoder : Decoder Config
configDecoder = Decode.map8 Config
  (field "maxHoliday" Decode.int)
  (field "maxLearningHoliday" Decode.int)
  (field "holidayURL" Decode.string)
  (field "fedState" Decode.string)
  (field "random" Decode.int)
  (Decode.maybe <| field "password" Decode.string)
  (field "loggedIn" Decode.bool)
  (field "pubHolList" pubHolYearListDecoder)

dbDecoder : Decoder TransferObj
dbDecoder = Decode.map3 TransferObj
  (field "config" configDecoder)
  (field "holList" holListDecoder)
  (field "init" Decode.bool)
