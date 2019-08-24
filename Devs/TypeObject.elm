module Devs.TypeObject exposing (..)

import Http
import Time exposing (Posix, Zone)
import File

import Devs.Objects as O exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | ReadDataFromPublish TransferObj
  | ShiftYear O.Shift
  | SetTimeZone Zone
  | SetYear Posix
  | SetPublicHolidays (Result Http.Error (List PublicHoliday))
  | ToggleConfigForm
  | ToggleHolidayForm
  | SetMaxHol String
  | SetLHol String
  | SetFedState String
  | SetUrl String
  | SetFrom String
  | SetTo String
  | SetType String
  | AddHoliday
  | RemoveHoliday String
  | DownloadDB
  | ImportDB
  | DBLoaded File.File
  | DBDecode String
