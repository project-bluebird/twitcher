module Twitcher.AltitudeForm

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
    CurrentAltitude : Altitude
    AltitudeUnit : AltitudeUnit
    NewAltitude : string
    VerticalSpeed : string option
    CheckFields : bool
   }

type Msg = 
  | ChangeAltitude of string
  | SetAltitudeUnit of string
  | SubmitForm
  | Cancel
  | Error
  | CheckFields

type ExternalMsg =
    | NoOp
    | Submit of AircraftID * Altitude * float option  // Submit altitude in feet  
    | Cancel

let init(aircraftID, currentAltitude) =
  { AircraftID = aircraftID
    NewAltitude = ""
    AltitudeUnit = Feet
    CurrentAltitude = currentAltitude
    VerticalSpeed = None
    CheckFields = false },
  Cmd.none    

let update msg model =
  match msg with
  | ChangeAltitude x ->
      { model with NewAltitude = x}, Cmd.none, NoOp

  | SetAltitudeUnit x ->
      Browser.console.log(x)
      let altUnit =
        match x with
        | "Flight levels" -> FlightLevels
        | "Meters" -> Meters
        | "Feet" -> Feet
        | _ -> Feet
      { model with AltitudeUnit = altUnit}, Cmd.none, NoOp

  | SubmitForm ->
      { model with CheckFields = true}, 
      Cmd.ofMsg CheckFields,
      NoOp

  | Msg.Cancel ->
      model, Cmd.none, ExternalMsg.Cancel

  | Error

  | CheckFields ->
      if (model.NewAltitude |> checkFloat) &&
         (if model.VerticalSpeed.IsSome then 
            model.VerticalSpeed.Value |> checkFloat 
          else true) then
        let altitude = 
          match model.AltitudeUnit with
          | FlightLevels -> (model.NewAltitude |> float |> round |> int) * 1<FL> |> Conversions.Altitude.fl2ft
          | Feet -> float model.NewAltitude * 1.<ft>
          | Meters -> float model.NewAltitude * 1.<m> |> Conversions.Altitude.m2ft
        let verticalSpeed = 
          model.VerticalSpeed 
          |> Option.map (fun value -> float value)
          
        model, Cmd.none, ExternalMsg.Submit (model.AircraftID, altitude, verticalSpeed)
      else
        model, Cmd.none, NoOp


let view model (dispatch: Msg -> unit) =
  Modal.modal [ Modal.IsActive true ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch Msg.Cancel) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ] [
              Heading.p [ Heading.Is5 ] [ str "Change altitude" ]

              Level.level [] 
                   [ Level.item [Level.Item.HasTextCentered ] [ 
                      div [] 
                        [ yield Level.heading [] [ str "Current altitude"]
                          let alt = model.CurrentAltitude
                          yield
                            (match model.AltitudeUnit with
                             | Feet -> Level.title [] [ str (sprintf "%.0f feet" alt) ]
                             | Meters -> Level.title [] [ str (sprintf "%.0f meters" (Conversions.Altitude.ft2m alt))]
                             | FlightLevels ->
                                Level.title [] [ str (sprintf "FL%d" (Conversions.Altitude.ft2fl alt)) ]
                             ) ]
                    ] ]

              form [ ]
                [ 
                  formItemOptions
                    "Altitude" 
                    [ "Feet"; "Flight levels"; "Meters" ]
                    SetAltitudeUnit
                    model.NewAltitude
                    ChangeAltitude 
                    model.CheckFields
                    checkFloat
                    "Altitude must be a number"
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



// TODO: more reasonable error checking - flight levels etc
