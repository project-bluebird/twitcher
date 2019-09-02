module Twitcher.App

open Twitcher.Domain
open Twitcher.Model
open Twitcher.CoordinateSystem
open Twitcher.Update
open Twitcher.View
open Twitcher.Commands

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome


open Elmish.React

open Fable.Import
open Fable.Core.JsInterop
open Thoth.Json
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
    SimulationSpeed = 1.0
    SimulationTime = System.TimeSpan.FromSeconds(0.0)
    SectorDisplay = TopDown
   },
  Cmd.batch [
    Cmd.ofMsg Init
    Cmd.ofMsg LoadSector
  ]
// TODO save state into cookie to stay through refresh?

open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
