port module Devs.Ports exposing (..)

import Devs.Objects as O exposing ( TransferObj, EleDimensions )

port pushDataToStore: TransferObj -> Cmd msg
port getDimOfElement: String -> Cmd msg

port setDataFromStore: (TransferObj -> msg) -> Sub msg
port setDimOfElement: (EleDimensions -> msg) -> Sub msg
