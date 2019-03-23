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

type SpeedUnit = Knots | Mach
type AltitudeUnit = FlightLevels | Feet | Meters

type FormModel = 
  { AircraftID : string
    Latitude : string
    Longitude : string
    Heading: string
    Speed : string
    SpeedUnit : string
    AltitudeUnit : string
    Altitude : string }

type Msg = 
  | ChangeAircraftID of string
  | ChangeLatitude of string
  | ChangeLongitude of string
  | ChangeHeading of string
  | ChangeSpeed of string
  | SetSpeedUnit of string
  | ChangeAltitude of string
  | SetAltitudeUnit of string
  

type ExternalMsg =
    | NoOp
    | Submit of AircraftInfo  

let init() =
  { AircraftID = ""
    Latitude = "" 
    Longitude = ""
    Heading = ""
    Speed = ""
    SpeedUnit = "Knots"
    Altitude = ""
    AltitudeUnit = "Flight Levels" },
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
      { model with SpeedUnit = su }, Cmd.none, NoOp

  | ChangeAltitude x ->
      { model with Altitude = x}, Cmd.none, NoOp

  | SetAltitudeUnit x ->
      Browser.console.log(x)
      { model with AltitudeUnit = x}, Cmd.none, NoOp

let formItem label textPlaceholder value message isValid warning (dispatch: Msg -> unit) =
  Field.div [ ]
    [ Label.label [ ] [ str label ]
      Input.text [ 
        Input.Placeholder textPlaceholder
        Input.Value value    
        Input.Props 
          [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch ) ] ] 
      
      Help.help [ (if isValid value then Help.Color IsGrey else Help.Color IsDanger) ]
        [ str warning ] ]        


let formItemOptions label (options: string list) optionMessage textPlaceholder value message isValid warning (dispatch: Msg -> unit) = 
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
            [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch ) ] ] 
      ]      
    Help.help [ (if isValid value then Help.Color IsGrey else Help.Color IsDanger) ]
        [ str warning ] ]


let view model (dispatch: Msg -> unit) =
  Container.container [] [
    Heading.p [ Heading.Is5 ] [ str "Create new aircraft" ]
    form [ ]
      [ formItem 
          "Aircraft identifier"
          "ABC123"
          model.AircraftID    
          ChangeAircraftID 
          checkAircraftID 
          "Aircraft ID must have at least 3 letters/numbers." 
          dispatch
        formItem 
          "Latitude [decimal degrees]" 
          "55" 
          model.Latitude
          ChangeLatitude
          checkFloat
          "Latitude must be a floating point number"
          dispatch
        formItem 
          "Longitude [decimal degrees]" 
          "0" 
          model.Longitude
          ChangeLongitude 
          checkFloat
          "Longitude must be a floating point number"
          dispatch
        formItem 
          "Heading [degrees]" 
          "0" 
          model.Heading
          ChangeHeading 
          checkFloat
          "Heading must be a floating point number"
          dispatch
        formItemOptions
          "Altitude" 
          [ "Flight levels"; "Feet"; "Meters" ]
          SetAltitudeUnit
          "0" 
          model.Altitude
          ChangeAltitude 
          checkFloat
          "Speed must be a number"
          dispatch          
        formItemOptions
          "Calibrated air speed"
          ["knots"; "Mach" ]
          SetSpeedUnit
          "0" 
          model.Speed
          ChangeSpeed 
          checkFloat
          "Speed must be a number"
          dispatch       

           
      ]

  ]


// TODO: parse info, submit, more reasonable error checking
