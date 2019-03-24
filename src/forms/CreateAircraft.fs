module Twitcher.AircraftForm

open Twitcher.Domain
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

type SpeedUnit = Knots | Mach | Kmh
type AltitudeUnit = FlightLevels | Feet | Meters

type FormModel = 
  { AircraftID : string
    Latitude : string
    Longitude : string
    Heading: string
    Speed : string
    SpeedUnit : SpeedUnit
    AltitudeUnit : AltitudeUnit
    Altitude : string
    CheckFields : bool
    Type : string }

type Msg = 
  | ChangeAircraftID of string
  | ChangeLatitude of string
  | ChangeLongitude of string
  | ChangeHeading of string
  | ChangeSpeed of string
  | SetSpeedUnit of string
  | ChangeAltitude of string
  | SetAltitudeUnit of string
  | SubmitForm
  | Cancel
  | Error
  | CheckFields
  

type ExternalMsg =
    | NoOp
    | Submit of AircraftInfo  
    | Cancel

let init() =
  { AircraftID = ""
    Latitude = "" 
    Longitude = ""
    Heading = ""
    Speed = ""
    SpeedUnit = Knots
    Altitude = ""
    Type = "B744"
    AltitudeUnit = Feet
    CheckFields = false },
  Cmd.none    

let checkAircraftID (x: string) =
  (x.Length >= 3) 
  && (x |> Seq.forall (fun c -> System.Char.IsLetterOrDigit(c)))

let checkFloat x =
  let canParse, result = System.Double.TryParse(x)
  canParse

let update msg model =
  match msg with
  | ChangeAircraftID x ->
    { model with 
        FormModel.AircraftID = x },
    Cmd.none,
    NoOp

  | ChangeLatitude x ->
      { model with Latitude = x }, Cmd.none, NoOp  
  
  | ChangeLongitude x ->
      { model with Longitude = x }, Cmd.none, NoOp  
  
  | ChangeHeading x ->
      { model with Heading = x }, Cmd.none, NoOp

  | ChangeSpeed x ->
      { model with Speed = x }, Cmd.none, NoOp  

  | SetSpeedUnit su ->
      let speedUnit =
        match su with
        | "Knots" -> Knots
        | "Km/h" -> Kmh
        | "Mach" -> Mach
        | _ -> Knots
      { model with SpeedUnit = speedUnit }, Cmd.none, NoOp

  | ChangeAltitude x ->
      { model with Altitude = x}, Cmd.none, NoOp

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
      if (model.AircraftID |> checkAircraftID) &&
        (model.Latitude |> checkFloat) &&
        (model.Longitude |> checkFloat) &&
        (model.Heading |> checkFloat) &&
        (model.Speed |> checkFloat) &&
        (model.Altitude |> checkFloat) then
        let aircraftInfo = 
          { AircraftID = model.AircraftID
            Type = Some model.Type
            Time = None
            Heading = Some (float model.Heading)
            Latitude = float model.Latitude
            Longitude = float model.Longitude
            Speed = 
              match model.SpeedUnit with
              | Knots -> CalibratedAirSpeed(CalibratedAirSpeed.Knots(float model.Speed))
              | Mach -> CalibratedAirSpeed(CalibratedAirSpeed.Mach(float model.Speed))
              | Kmh -> CalibratedAirSpeed(CalibratedAirSpeed.Knots(float model.Speed/1.852))
            Altitude = 
              match model.AltitudeUnit with
              | FlightLevels -> FlightLevel(int (model.Altitude |> float |> round |> int))
              | Feet -> Altitude(float model.Altitude) 
              | Meters -> Altitude(float model.Altitude * 3.281)}
        model, Cmd.none, ExternalMsg.Submit aircraftInfo
      else
        model, Cmd.none, NoOp

let formItem label textPlaceholder value message checkValid isValid warning other (dispatch: Msg -> unit) =
  Field.div [ ]
    [ yield!
        [ Label.label [ ] [ str label ] ]
      yield!
        [Input.text [ 
          Input.Placeholder textPlaceholder
          Input.Value value    
          Input.Props 
            [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch ) ] ] ]
      
      yield!
        [Help.help 
          [ (if checkValid && not (isValid value) then Help.Color IsDanger else Help.Color IsGrey) ]
          [ str warning ]]
      yield! 
        (match other with 
         | Some(elem) -> [elem]
         | None -> [] )]        


let formItemOptions label (options: string list) optionMessage textPlaceholder value message checkValid isValid warning (dispatch: Msg -> unit) = 
  Field.div [] [
    Label.label [ ] [ str label ]
  
    Field.div [ Field.HasAddons ]
      [ Select.select [ ]
          [ select [ DefaultValue (options.Head)
                     OnChange (fun ev -> !!ev.target?value |> optionMessage |> dispatch ) ]
              (options
               |> List.map (fun value ->
                  option [ Value value ][ str value] ))
          ] 
        Input.text [ 
          Input.Placeholder textPlaceholder
          Input.Value value    
          Input.Props 
            [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch )
               ] ] 
      ]      
    Help.help 
        [ (if checkValid && not (isValid value) then Help.Color IsDanger else Help.Color IsGrey) ]
        [ str warning ] ]



let view model (dispatch: Msg -> unit) =
  Modal.modal [ Modal.IsActive true ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch Msg.Cancel) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ] [
              Heading.p [ Heading.Is5 ] [ str "Create new aircraft" ]
              form [ ]
                [ formItem 
                    "Aircraft identifier"
                    "ABC123"
                    model.AircraftID    
                    ChangeAircraftID 
                    model.CheckFields
                    checkAircraftID 
                    "Aircraft ID must have at least 3 letters/numbers." 
                    None
                    dispatch 

                  formItem 
                    "Aircraft type (ICAO identifier)"
                    "B744"
                    model.AircraftID    
                    ChangeAircraftID 
                    model.CheckFields
                    checkAircraftID 
                    "Aircraft type, for example B744 is Boeing 747-400." 
                    (Some( 
                      Text.span 
                        [ Modifiers [Modifier.TextSize (Screen.All, TextSize.Is7) ]] 
                        [ a [ Href "https://en.wikipedia.org/wiki/List_of_ICAO_aircraft_type_designators"
                              Target "_blank"
                            ] [ str "Wikipedia - list of ICAO aircraft identifiers" ] 
                            ]))
                    dispatch  

                  formItem 
                    "Latitude [decimal degrees]" 
                    "55" 
                    model.Latitude
                    ChangeLatitude
                    model.CheckFields
                    checkFloat
                    "Latitude must be a floating point number"
                    None
                    dispatch

                  formItem 
                    "Longitude [decimal degrees]" 
                    "0" 
                    model.Longitude
                    ChangeLongitude 
                    model.CheckFields
                    checkFloat
                    "Longitude must be a floating point number"
                    None
                    dispatch

                  formItem 
                    "Heading [degrees]" 
                    "0" 
                    model.Heading
                    ChangeHeading 
                    model.CheckFields
                    checkFloat
                    "Heading must be a floating point number"
                    None
                    dispatch

                  formItemOptions
                    "Altitude" 
                    [ "Feet"; "Flight levels"; "Meters" ]
                    SetAltitudeUnit
                    "0" 
                    model.Altitude
                    ChangeAltitude 
                    model.CheckFields
                    checkFloat
                    "Altitude must be a number"
                    dispatch          
                  formItemOptions
                    "Calibrated air speed"
                    ["Knots"; "Mach"; "Km/h" ]
                    SetSpeedUnit
                    "0" 
                    model.Speed
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



// TODO: more reasonable error checking - flight levels etc
