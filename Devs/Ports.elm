port module Devs.Ports exposing (..)

import Devs.Objects as O exposing ( TransferObj )

port pushDataToStore: TransferObj -> Cmd msg

port setDataFromStore: (TransferObj -> msg) -> Sub msg
