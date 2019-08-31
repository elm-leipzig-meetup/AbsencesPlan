module Devs.Update exposing (..)

import Time as T
import Task
import UUID exposing (UUID)
import Random
import SHA1
import File
import File.Download as D exposing ( string )
import File.Select as S exposing ( file )
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Debug exposing (log)

import Devs.Ports as P exposing (pushDataToStore, getDimOfElement)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing ( updatePublicHolidayListInConfig, fillYear, stringToDate )
import Devs.DatabaseEncode as HE exposing (dbEncoder)
import Devs.DatabaseDecode as HD exposing (dbDecoder)

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
        GetDomDim id -> ( model , P.getDimOfElement id)
        SetDomDim dim -> ( { model | pageWidth=dim.width, pageHeight=dim.height }, Cmd.none)
        ReadDataFromPublish obj -> ( { model | config=obj.config, holList=obj.holList } , Task.perform TO.SetTimeZone T.here)
        SetMaxHol val ->
          let
            c = model.config
          in
              ( { model | config = { c | maxHoliday = Maybe.withDefault 0 (String.toInt val) } } , Cmd.none)
        SetLHol val ->
          let
            c = model.config
          in
              ( { model | config = { c | maxLearningHoliday = Maybe.withDefault 0 (String.toInt val) } } , Cmd.none)
        SetFedState val ->
          let
            c = model.config
          in
              ( { model | config = { c | fedState = val } } , Cmd.none)
        SetUrl val ->
          let
            c = model.config
          in
              ( { model | config = { c | holidayURL = val } } , Cmd.none)
        SetPW val -> ( { model | tmpPw = (if String.isEmpty val then Nothing else Just val) } , Cmd.none)
        ShiftYear op ->
          let
            newYear = case op of
              Up -> model.currentYear + 1
              Down -> model.currentYear - 1
            m = { model | currentYear = newYear }
          in
            ( m , DU.setPublicHolidays m)
        SetTimeZone zone -> ( { model | timeZone = zone } , Task.perform TO.SetYear T.now)
        SetYear ts ->
          let
            m = if model.currentYear > 1977 then model
              else { model | currentYear = T.toYear model.timeZone ts }
          in
            ( m , DU.setPublicHolidays m)
        SetFrom val ->
          let
            tmpHol = case model.tmpHol of
              Just holiday -> holiday
              Nothing -> O.getEpmtyHoliday
            hol = { tmpHol | from=DU.stringToDate val }
          in
            ( { model | tmpHol = Just hol } , Cmd.none)
        SetTo val ->
          let
            tmpHol = case model.tmpHol of
              Just holiday -> holiday
              Nothing -> O.getEpmtyHoliday
            hol = { tmpHol | to=DU.stringToDate val }
          in
            ( { model | tmpHol = Just hol } , Cmd.none)
        SetType val ->
          let
            tmpHol = case model.tmpHol of
              Just holiday -> holiday
              Nothing -> O.getEpmtyHoliday
            holType = DU.intToHoltype (Maybe.withDefault 0 (String.toInt val))
            hol = { tmpHol | holType=DU.holTypeToInt holType }
          in
            ( { model | tmpHol = Just hol } , Cmd.none)
        AddHoliday ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
          in
            case model.tmpHol of
              Just holiday -> ( { model | currentSeed = Just newSeed, holList = List.append model.holList [{ holiday | uuid=UUID.toString newUuid }], tmpHol = Nothing } , Cmd.none)
              Nothing -> ( model, Cmd.none)
        RemoveHoliday uuid -> ( { model | holList=List.filter (\i -> i.uuid /= uuid) model.holList }, Cmd.none)
        SetPublicHolidaysFromCache list -> ( { model | config = (DU.updatePublicHolidayListInConfig model.config {year=model.currentYear, fedState=model.config.fedState, pubHolList=list}), calendar = DU.fillYear model.currentYear model.timeZone list model.holList } , Cmd.none)
        SetPublicHolidays (Ok list) -> ( { model | config = (DU.updatePublicHolidayListInConfig model.config {year=model.currentYear, fedState=model.config.fedState, pubHolList=list}), calendar = DU.fillYear model.currentYear model.timeZone list model.holList } , Cmd.none)
        SetPublicHolidays (Err error) -> ( { model | calendar = DU.fillYear model.currentYear model.timeZone [] model.holList } , Cmd.none)
        ToggleConfigForm ->
          let
            c = model.config
            c1 = if model.tmpPw /= Nothing && not (String.isEmpty (Maybe.withDefault "" model.tmpPw ))
              then { c | password = Just (SHA1.toBase64 (SHA1.fromString (Maybe.withDefault "" model.tmpPw ))) }
              else c
          in
            ( { model | showConfigForm = not model.showConfigForm, tmpPw=Nothing, config = c1 } , Cmd.batch [P.pushDataToStore {config=c1, holList=model.holList, init=False}, DU.setPublicHolidays model] )
        ToggleLoginForm ->
          let
            c = model.config
            hash = if model.tmpPw /= Nothing && not (String.isEmpty (Maybe.withDefault "" model.tmpPw ))
              then (SHA1.toBase64 (SHA1.fromString (Maybe.withDefault "" model.tmpPw )))
              else ""
            c1 = if hash == (Maybe.withDefault "" model.config.password)
              then { c | loggedIn = True }
              else c
          in
            ( { model | showLoginForm = not model.showLoginForm, tmpPw=Nothing, config=c1 }
              , P.pushDataToStore {config=c1, holList=model.holList, init=False} )
        Logout ->
          let
            c = model.config
          in
            ( { model | config = { c | loggedIn = not model.config.loggedIn } }
              , P.pushDataToStore {config={ c | loggedIn = not model.config.loggedIn }, holList=model.holList, init=False} )
        ToggleHolidayForm ->
          let
            tmpHol = if not model.showHolidayForm
              then Just O.getEpmtyHoliday
              else Nothing
            cmds = if model.showHolidayForm
              then Cmd.batch [P.pushDataToStore {config=model.config, holList=model.holList, init=False}, DU.setPublicHolidays model]
              else Cmd.none
          in
            ( { model | showHolidayForm = not model.showHolidayForm, tmpHol = tmpHol }, cmds )
        DownloadDB -> ( model, D.string "holidayDB.json" "application/json" (Encode.encode 0 (HE.dbEncoder {config=model.config, holList=model.holList, init=False})))
        ImportDB -> ( model, S.file ["application/json"] DBLoaded )
        DBLoaded file -> ( model , Task.perform DBDecode (File.toString file))
        DBDecode content ->
          case Decode.decodeString HD.dbDecoder content of
            Ok obj -> ( { model | config=obj.config, holList=obj.holList } , Cmd.batch [P.pushDataToStore {config=obj.config, holList=obj.holList, init=False}, DU.setPublicHolidays { model | config=obj.config, holList=obj.holList }])
            Err error -> ( model , Cmd.none)
