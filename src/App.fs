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

  {     
    SimulationInfo = None
    Positions = []
    Animate = false
    Config = None
    SectorInfo = None
    SectorDisplay = TopDown
    DisplayView = {
      VisualisationViewSize = 0., 0.
      DisplayArea = sectorDisplayArea
    }
    ShowWaypoints = true
    State = NotConnected
    FormModel = None
    SimulationViewSize = 0.,0.
    SimulationZoom = 0.3
    ViewDetails = None
    PositionHistory = 0, (Dictionary<AircraftID, Position []>())
    InConflict = [||]
    SeparationDistance = None
    SimulationSpeed = 1.0
    SimulationTime = System.TimeSpan.FromSeconds(0.0)

    Score = 0.
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
