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

open Twitcher
open Twitcher.Domain
open Twitcher.Model
open Twitcher.CoordinateSystem
open Twitcher.Commands



let delayMsg _ =
  promise {
    do! Promise.sleep 1000
    return ()
  } 


let simulationViewSize() = 
  Browser.document.getElementById("simulation-viewer").clientWidth,
  Browser.document.getElementById("simulation-viewer").clientHeight  


let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    | Init ->
        model, 
        Cmd.batch [
          getConfigCmd()
          Cmd.ofMsg GetSimulationViewSize
        ]
    
    | ConnectionActive result ->
        if result then 
          { model with State = Connected }, Cmd.none
        else 
          { model with State = ConnectionFailed}, Cmd.none

    | Config config ->
       { model with Config = Some config }, 
       pingBluebirdCmd config
       
    | GetSimulationViewSize ->
        { model with SimulationViewSize = simulationViewSize()},
        Cmd.none

    | ViewAircraftDetails aircraftID ->
        { model with ViewDetails = Some(aircraftID) }, Cmd.none

    | CloseAircraftDetails ->
        { model with ViewDetails = None }, Cmd.none

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
        { model with Positions = positionInfo |> List.ofArray } ,
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
        // pause the scenario
        { model with
            State = ActiveSimulation Playing }, 
        pauseSimulationCmd model.Config.Value

    | ResetSimulator -> 
        model, 
        Cmd.batch [
          resetSimulatorCmd model.Config.Value
          Cmd.ofMsg CloseAircraftDetails
        ]

    | ResetedSimulator result -> 
        if not result then 
          Browser.console.log("Failed to reset the simulator")
          model, Cmd.none
        else
          { model with State = Connected }, Cmd.none

    | PauseSimulation -> 
        model, pauseSimulationCmd model.Config.Value

    | PausedSimulation result -> 
        if not result then 
          Browser.console.log("Failed to pause the simulation")
          model, Cmd.none
        else 
          { model with State = ActiveSimulation Paused }, Cmd.ofMsg StopAnimation

    | ResumeSimulation -> 
        model, resumeSimulationCmd model.Config.Value

    | ResumedSimulation result ->         
        if not result then 
          Browser.console.log("Failed to resume the simulation")
          model, Cmd.none
        else 
          { model with State = ActiveSimulation Playing }, Cmd.ofMsg StartAnimation

    | SetSimulationRateMultiplier rm -> model, Cmd.none
    | ChangedSimulationRateMultiplier -> model, Cmd.none

    | CreateAircraft aircraftInfo -> 
        model, createAircraftCmd model.Config.Value aircraftInfo

    | CreatedAircraft result -> 
        Browser.console.log(result)
        model, Cmd.none

    | ChangeAltitude (aircraftID, requestedAltitude, verticalSpeed) -> 
        model, changeAltitudeCmd model.Config.Value aircraftID requestedAltitude verticalSpeed

    | ChangedAltitude result -> 
        Browser.console.log(result)
        model, Cmd.none

    | ChangeHeading (aircraftID, requestedHeading) -> model, Cmd.none
    | ChangedHeading -> model, Cmd.none

    | ChangeSpeed (aircraftID, speed) -> 
        model, changeSpeedCmd model.Config.Value aircraftID speed
    | ChangedSpeed result -> 
        Browser.console.log(result)
        model, Cmd.none

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
           Cmd.ofMsg GetSimulationViewSize
          ]
        else
          model,
          Cmd.none

    | StartAnimation ->
        { model with Animate = true }, Cmd.ofMsg (MakeStep())

    | StopAnimation ->
        { model with Animate = false }, Cmd.none

    | ShowCreateAircraftForm ->
        let f, cmd = AircraftForm.init()
        { model with FormModel = Some (CreateAircraftForm(f)) }, 
            Cmd.batch [
              Cmd.map CreateAircraftMsg cmd
            ]       

    | ShowChangeAltitudeForm aircraft ->
        let f, cmd = AltitudeForm.init(aircraft.AircraftID, aircraft.Position.Altitude)
        { model with FormModel = Some (ChangeAltitudeForm(f)) }, 
            Cmd.batch [
              Cmd.map ChangeAltitudeMsg cmd
            ]        

    | ShowChangeSpeedForm aircraft ->
        let f, cmd = SpeedForm.init(aircraft.AircraftID, aircraft.GroundSpeed)
        { model with FormModel = Some (ChangeSpeedForm(f)) }, 
            Cmd.batch [
              Cmd.map ChangeSpeedMsg cmd
            ]                  

    | CreateAircraftMsg m ->
      match model.FormModel with

      | Some(CreateAircraftForm f) ->
          let f', cmd, externalMsg = AircraftForm.update m f

          match externalMsg with
          | AircraftForm.ExternalMsg.Submit info ->
              { model with FormModel = None }, 
              Cmd.batch [
                Cmd.ofMsg (CreateAircraft info)
              ]

          | AircraftForm.ExternalMsg.NoOp ->
              { model with FormModel = Some (CreateAircraftForm(f')) }, 
              Cmd.map CreateAircraftMsg cmd

          | AircraftForm.ExternalMsg.Cancel ->
              { model with FormModel = None },
              Cmd.none
              
      | None | Some _ ->
          let f, cmd = AircraftForm.init()  
          { model with FormModel = Some (CreateAircraftForm(f)) }, 
              Cmd.batch [
                Cmd.map CreateAircraftMsg cmd
                Cmd.ofMsg (CreateAircraftMsg m) // initialized, resend
              ]          

    | ChangeAltitudeMsg m ->
      match model.FormModel with

      | Some(ChangeAltitudeForm f) ->
          let f', cmd, externalMsg = AltitudeForm.update m f

          match externalMsg with
          | AltitudeForm.ExternalMsg.Submit(acid,alt,vs) ->
              { model with FormModel = None }, 
              Cmd.batch [
                Cmd.ofMsg (ChangeAltitude (acid,alt,vs))
              ]

          | AltitudeForm.ExternalMsg.NoOp ->
              { model with FormModel = Some (ChangeAltitudeForm(f')) }, 
              Cmd.map ChangeAltitudeMsg cmd

          | AltitudeForm.ExternalMsg.Cancel ->
              { model with FormModel = None },
              Cmd.none
              
      | None | Some _ ->
          Browser.console.log("Error - incorrect form model")
          { model with FormModel = None }, Cmd.none     

    | ChangeSpeedMsg m ->
      match model.FormModel with

      | Some(ChangeSpeedForm f) ->
          let f', cmd, externalMsg = SpeedForm.update m f

          match externalMsg with
          | SpeedForm.ExternalMsg.Submit(acid,cas) ->
              { model with FormModel = None }, 
              Cmd.batch [
                Cmd.ofMsg (ChangeSpeed (acid,cas))
              ]

          | SpeedForm.ExternalMsg.NoOp ->
              { model with FormModel = Some (ChangeSpeedForm(f')) }, 
              Cmd.map ChangeSpeedMsg cmd

          | SpeedForm.ExternalMsg.Cancel ->
              { model with FormModel = None },
              Cmd.none
              
      | None | Some _ ->
          Browser.console.log("Error - incorrect form model")
          { model with FormModel = None }, Cmd.none     