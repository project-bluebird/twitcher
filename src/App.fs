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
  // default sector display area is the college airspace
  let sectorDisplayArea = {
    BottomLeft = CoordinateSystem.Mercator.lonLatToXY -2.5 50.3 //-3.974833333 49.0675 
    TopRight = CoordinateSystem.Mercator.lonLatToXY 1.4 52.0 //5.090666667 53.80883333
    BottomAltitude = 0.0<ft>
    TopAltitude = 45000.<ft>
  }

  { Positions = []
    Animate = false
    Config = None
    Sector = None
    State = NotConnected
    FormModel = None
    SectorView = {
      VisualisationViewSize = 0.,0.
      SectorDisplayArea = sectorDisplayArea
    }
    ViewDetails = None
    PositionHistory = 0, (Dictionary<AircraftID, Position []>())
    InConflict = [||]
    SeparationDistance = None
    SimulationSpeed = 1.0
    SimulationTime = System.TimeSpan.FromSeconds(0.0)
    SectorDisplay = TopDown
    TeamCount = 0
    TeamScores = [|0.|]
   },
  Cmd.batch [
    Cmd.ofMsg Init
  ]
// TODO save state into cookie to stay through refresh?

open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
