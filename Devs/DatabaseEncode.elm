module Devs.DatabaseEncode exposing ( dbEncoder )

import Json.Encode as Encode exposing (..)

import Devs.Objects as Objects exposing (..)

dateEncoder: Date -> Encode.Value
dateEncoder obj =
    Encode.object [
      ( "day", Encode.int obj.day )
      , ( "month", Encode.int obj.month )
      , ( "year", Encode.int obj.year )
    ]

holidayEncoder: Holiday -> Encode.Value
holidayEncoder obj =
    Encode.object [
      ( "from", dateEncoder obj.from )
      , ( "to", dateEncoder obj.to )
      , ( "holType", Encode.int obj.holType )
      , ( "uuid", Encode.string obj.uuid )
    ]

configEncoder: Config -> Encode.Value
configEncoder obj =
    Encode.object [
      ( "maxHoliday", Encode.int obj.maxHoliday )
      , ( "maxLearningHoliday", Encode.int obj.maxLearningHoliday )
      , ( "holidayURL", Encode.string obj.holidayURL )
      , ( "fedState", Encode.string obj.fedState )
      , ( "random", Encode.int obj.random )
    ]

dbEncoder: TransferObj -> Encode.Value
dbEncoder obj =
    let
      list =
          [ ( "config", configEncoder obj.config )
          , ( "holList", Encode.list holidayEncoder obj.holList )
          , ( "init", Encode.bool obj.init )
          ]
    in
      Encode.object list
