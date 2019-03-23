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

type FormModel = 
  { AircraftID : string }

type Msg = 
  | SetAircraftID of string
  

type ExternalMsg =
    | NoOp
    | Submit of AircraftInfo  

let init() =
  { AircraftID = "" },
  Cmd.none    

let update msg model =
  match msg with
  | SetAircraftID x ->
    { model with FormModel.AircraftID = x },
    Cmd.none,
    NoOp

let view model (dispatch: Msg -> unit) =
  Heading.p [] [ str "Hello from Create Aircraft Form" ]

