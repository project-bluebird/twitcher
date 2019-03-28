module Twitcher.SpeedForm

open Twitcher.Domain
open Twitcher.Form

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome


open Elmish.React

open Fable.Import
open Fable.PowerPack
open Fable.Core.JsInterop
open Thoth.Json
open Fable.PowerPack.Fetch.Fetch_types

type FormModel = 
  { AircraftID : string
    GroundSpeed : Speed option
    SpeedUnit : SpeedUnit
    NewSpeed : string
    VerticalSpeed : string option
    CheckFields : bool
   }

type Msg = 
  | ChangeSpeed of string
  | SetSpeedUnit of string
  | SubmitForm
  | Cancel
  | Error
  | CheckFields

type ExternalMsg =
    | NoOp
    | Submit of AircraftID * Speed 
    | Cancel

let init(aircraftID, groundSpeed) =
  { AircraftID = aircraftID
    NewSpeed = ""
    SpeedUnit = Knots
    GroundSpeed = groundSpeed
    VerticalSpeed = None
    CheckFields = false },
  Cmd.none    

let update msg model =
  match msg with
  | ChangeSpeed x ->
      { model with NewSpeed = x}, Cmd.none, NoOp

  | SetSpeedUnit x ->
      Browser.console.log(x)
      let speedUnit =
        match x with
        | "Km/h" -> Kmh
        | "Knots" -> Knots
        | "Mach" -> Mach
        | _ -> Knots
      { model with SpeedUnit = speedUnit}, Cmd.none, NoOp

  | SubmitForm ->
      { model with CheckFields = true}, 
      Cmd.ofMsg CheckFields,
      NoOp

  | Msg.Cancel ->
      model, Cmd.none, ExternalMsg.Cancel

  | Error

  | CheckFields ->
      if (model.NewSpeed |> checkFloat) &&
         (if model.VerticalSpeed.IsSome then 
            model.VerticalSpeed.Value |> checkFloat 
          else true) then
        let speed = 
          match model.SpeedUnit with
          | Kmh -> float model.NewSpeed * 1.<km/h> |> Conversions.Speed.kmh2knot 
          | Knots -> float model.NewSpeed * 1.<knot>
          | Mach -> float model.NewSpeed * 1.<Mach> |> Conversions.Speed.mach2knot
          | FeetPerMinute -> float model.NewSpeed * 1.<ft/minute> |> Conversions.Speed.fm2ms |> Conversions.Speed.ms2knot 
          | MetersPerSecond -> float model.NewSpeed * 1.<m/s> |> Conversions.Speed.ms2knot

        model, Cmd.none, ExternalMsg.Submit (model.AircraftID, speed)
      else
        model, Cmd.none, NoOp


let view model (dispatch: Msg -> unit) =
  Modal.modal [ Modal.IsActive true ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch Msg.Cancel) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ] [
              Heading.p [ Heading.Is5 ] [ str "Change calibrated air speed" ]
              Level.level [] 
                (match model.GroundSpeed with 
                 | Some(gs) ->
                   [ Level.item [Level.Item.HasTextCentered ] [ 
                      div [] 
                        [ Level.heading [] [ str "Current ground speed"]
                          (match model.SpeedUnit with
                           | Knots -> 
                              Level.title [] [ str (sprintf "%.0f knots" gs) ]
                           | Kmh -> 
                              Level.title [] [ str (sprintf "%.0f km/h" (gs |> Conversions.Speed.knot2kmh)) ]
                           | Mach -> 
                              Level.title [] [ str (sprintf "Mach %.3f" (gs |> Conversions.Speed.knot2mach)) ]
                           | FeetPerMinute | MetersPerSecond -> Level.title [] [ str "Error" ]
                           ) ]
                    ] ]
                 | None -> [])
              form [ ]
                [ // TODO display current speed?
                  formItemOptions
                    "Calibrated air speed (CAS)" 
                    [ "Knots"; "Mach"; "Km/h" ]
                    SetSpeedUnit
                    model.NewSpeed
                    ChangeSpeed
                    model.CheckFields
                    checkFloat
                    "Speed must be a number"
                    dispatch          
                ]
              hr []
              Button.button 
                  [ Button.OnClick (fun _ -> dispatch SubmitForm)
                    Button.Color Color.IsPrimary ]
                  [str "Submit"]
              Button.button 
                  [ Button.OnClick (fun _ -> dispatch Msg.Cancel)
                    Button.Color Color.IsGrey ]
                  [str "Cancel"]         
          ]
          ]
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick (fun _ -> dispatch Msg.Cancel) ] [ ] ]   

