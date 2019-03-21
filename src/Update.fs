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
    | Config config ->
       { model with Config = Some config }, Cmd.none
       
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
        { model with State = coordinates } ,
        Cmd.none
    
    | FetchedPosition positionInfo ->
        match positionInfo with
        | Some position ->
          Browser.console.log(position)
        | None ->
          Browser.console.log("Aircraft not found")
          
        model,
        Cmd.none

    | FetchError exn | ErrorMessage exn ->
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


