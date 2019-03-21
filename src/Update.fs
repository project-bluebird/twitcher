module Twitcher.Update

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React

open Fable.Import
open Fable.PowerPack
open Fable.Core.JsInterop
open Thoth.Json
open Fable.PowerPack.Fetch.Fetch_types

open Twitcher.Domain
open Twitcher.CoordinateSystem
open Twitcher.Commands



let delayMsg _ =
  promise {
    do! Promise.sleep 1000
    return ()
  }


let update (msg:Msg) (model:Model) =
    match msg with
    | Init ->
        model, 
        getConfigCmd()
    
    | ConnectionActive result ->
        if result then 
          { model with State = ConnectionEstablished }, Cmd.none
        else 
          { model with State = ConnectionFailed}, Cmd.none

    | Config config ->
       { model with Config = Some config }, 
       pingBluebirdCmd config
       
    | GetAllPositions ->
        match model.Config with
        | None ->
            Browser.console.log("No configuration found")
            model, Cmd.none
        | Some config ->
            model,
            getAllPositionsCmd config 

    | GetPosition aircraftID ->
        match model.Config with
        | None ->
            Browser.console.log("No configuration found")
            model, Cmd.none
        | Some config ->
            model,
            getAircraftPositionCmd config aircraftID

    | FetchedAllPositions positionInfo ->
        let coordinates = 
          positionInfo
          |> Array.map (fun pos -> 
              let (x,y) = 
                lonlatToMercator pos.Longitude pos.Latitude
                |> rescaleTest 
              { X = x; Y = y; Altitude = pos.Altitude })
          |> List.ofArray
        { model with Positions = coordinates } ,
        Cmd.none
    
    | FetchedPosition positionInfo ->
        match positionInfo with
        | Some position ->
          Browser.console.log(position)
        | None ->
          Browser.console.log("Aircraft not found")

        model,
        Cmd.none

    | LoadScenario path -> 
        model, 
        loadScenarioCmd model.Config.Value path
        
    | LoadedScenario response -> 
        Browser.console.log(response)
        model, Cmd.none

    | ResetSimulation -> model, Cmd.none
    | ResetedSimulation  -> model, Cmd.none

    | PauseSimulation -> model, Cmd.none
    | PausedSimulation -> model, Cmd.none

    | ResumeSimulation -> model, Cmd.none
    | ResumedSimulation -> model, Cmd.none

    | SetSimulationRateMultiplier rm -> model, Cmd.none
    | ChangedSimulationRateMultiplier -> model, Cmd.none

    | CreateAircraft aircraftInfo -> model, Cmd.none
    | CreatedAircraft -> model, Cmd.none

    | ChangeAltitude (aircraftID, requestedAltitude, verticalSpeed) -> model, Cmd.none
    | ChangedAltitude -> model, Cmd.none

    | ChangeHeading (aircraftID, requestedHeading) -> model, Cmd.none
    | ChangedHeading -> model, Cmd.none

    | ChangeVerticalSpeed (aircraftID, verticalSpeed) -> model, Cmd.none
    | ChangedVerticalSpeed  -> model, Cmd.none


    | ConnectionError exn | ErrorMessage exn ->
        Browser.console.error(exn)
        model,
        Cmd.none

    | MakeStep _ ->
        if model.Animate then
          model,
          Cmd.batch [
           getAllPositionsCmd model.Config.Value
           Cmd.ofPromise delayMsg () MakeStep ErrorMessage
          ]
        else
          model,
          Cmd.none

    | StartAnimation ->
        { model with Animate = true }, Cmd.ofMsg (MakeStep())

    | StopAnimation ->
        { model with Animate = false }, Cmd.none


