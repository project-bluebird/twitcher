module Twitcher.App

open Twitcher.Domain
open Twitcher.Model
open Twitcher.CoordinateSystem
open Twitcher.Update
open Twitcher.View
open Twitcher.Commands

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
open System.Collections.Generic


let init() =
  { Positions = []
    Animate = false 
    Config = None
    Sector = None
    State = NotConnected
    FormModel = None
    SimulationViewSize = 0.,0.
    ViewDetails = None
    PositionHistory = 0, (Dictionary<AircraftID, Position []>())
    InConflict = [||]
    SeparationDistance = None
   },
  Cmd.batch [
    Cmd.ofMsg Init
    Cmd.ofMsg LoadSector
  ]  
// TODO save state into cookie to stay through refresh?

open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
|> Program.withConsoleTrace
|> Program.run
