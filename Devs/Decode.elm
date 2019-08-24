module Devs.Decode exposing ( holidayDecoder )

import Json.Decode as Decode exposing (Decoder, field, succeed)

import Devs.Objects as Objects exposing (..)

holidayDecoder : Decoder PublicHoliday
holidayDecoder = Decode.map2 PublicHoliday
  (field "title" Decode.string)
  (field "date" Decode.string)
