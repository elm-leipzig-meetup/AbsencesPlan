module Devs.Objects exposing (..)

import Time exposing (Zone)
import UUID exposing (UUID)
import Random

type Shift = Up | Down
type HolType = Ill | Kar | Hol | LHol

-- Model
type alias Model = {
  currentYear: Int
  , calendar: Year
  , timeZone: Zone
  , config: Config
  , holList: List Holiday
  , tmpHol: Maybe Holiday
  , showConfigForm: Bool
  , showHolidayForm: Bool
  , currentSeed: Maybe Random.Seed
  }

type alias TransferObj = {
  config: Config
  , holList: List Holiday
  , init: Bool
  }

type alias Holiday = {
  from: Date
  , to: Date
  , holType: Int
  , uuid: String
  }

type alias Config = {
  maxHoliday: Int
  , maxLearningHoliday: Int
  , holidayURL: String
  , fedState: String
  , random: Int
  }

type alias Year = {
  name: Int
  , months: List Month
  }

type alias Week = {
  nr: Int
  , weekDays: List WeekDay
  }

type alias WeekDay = {
  name: String
  , day: Day
  }

type alias Month = {
  name: String
  , nr: Int
  , days: List Day
  , weeks: List Week
  }

type alias Day = {
  date: Date
  , holidayName: Maybe String
  , holType: Maybe Int
  }

type alias Date = {
  day: Int
  , month: Int
  , year: Int
  }

type alias PublicHoliday = {
  title: String
  , date: String
  }

--Model
initialModel: Model
initialModel = {
  currentYear = 1977
  , currentSeed = Nothing
  , timeZone = Time.utc
  , calendar = {
    name = 2019
    , months=[]
  }
  , config = getEmptyConfig
  , holList=[]
  , tmpHol=Nothing
  , showConfigForm=False
  , showHolidayForm=False
  }

getEmptyConfig = {
  maxHoliday=30
  , maxLearningHoliday=5
  , holidayURL="https://ipty.de/feiertag/api.php?do=getFeiertage&loc=[fedState]&outformat=Y-m-d&jahr=[year]"
  , fedState = "NI"
  , random=0
  }

getEpmtyHoliday = {
  from={day=0,month=0,year=0}
  , to={day=0,month=0,year=0}
  , holType=2
  , uuid=UUID.toString UUID.nil
  }

getDefaultHoliday = Hol
