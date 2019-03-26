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


let init() =
  let sectorFile = "data/sector31-outline.csv"
  let sectorOutline =
    if System.IO.File.Exists sectorFile then
      System.IO.File.ReadAllLines sectorFile
      |> fun a -> a.[1..]
      |> Array.map (fun line -> line.Split ',' |> Array.map float |> fun a -> a.[0], a.[1])
      |> Some
    else 
      None
  { Positions = []
    Animate = false 
    Config = None
    Sector = sectorOutline
    State = NotConnected
    FormModel = None
    SimulationViewSize = 0.,0.
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
