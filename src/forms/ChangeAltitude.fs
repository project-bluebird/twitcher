module Twitcher.AltitudeForm

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
  { AircraftID : string
    CurrentAltitude : float
    AltitudeUnit : AltitudeUnit
    NewAltitude : string
    CheckFields : bool
   }

