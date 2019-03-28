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
    Sector = 
      NATS.sectorOutline 
      |> List.map (fun (lat,lon) -> {Latitude = lat * 1.<latitude>; Longitude = lon * 1.<longitude>}) 
      |> Some  // TODO: load sector in a more principled way!
    State = NotConnected
    FormModel = None
    SimulationViewSize = 0.,0.
    ViewDetails = None
    PositionHistory = 0, (Dictionary<AircraftID, Position []>())
   },
  Cmd.none
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
