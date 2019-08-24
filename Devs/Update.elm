module Devs.Update exposing (..)

import Time as T
import Task
import UUID exposing (UUID)
import Random

import Devs.Ports as P exposing (pushDataToStore)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing ( fillYear, stringToDate )

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
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
            m = { model | currentYear = T.toYear model.timeZone ts }
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
        SetPublicHolidays (Ok list) ->
          ( { model | calendar = DU.fillYear model.currentYear model.timeZone list model.holList } , Cmd.none)
        SetPublicHolidays (Err error) ->
          ( model , Cmd.none)
--          ( { model | alertMessage = Just (DU.httpErrorToMessage error) }, Cmd.none)
        ToggleConfigForm -> ( { model | showConfigForm = not model.showConfigForm } , Cmd.batch [P.pushDataToStore {config=model.config, holList=model.holList, init=False}, DU.setPublicHolidays model] )
        ToggleHolidayForm ->
          let
            tmpHol = if not model.showHolidayForm
              then Just O.getEpmtyHoliday
              else Nothing
          in
            ( { model | showHolidayForm = not model.showHolidayForm, tmpHol = tmpHol } , Cmd.batch [P.pushDataToStore {config=model.config, holList=model.holList, init=False}, DU.setPublicHolidays model] )