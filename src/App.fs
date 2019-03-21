module Twitcher.App

open Twitcher.Domain
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
  { State = []
    Animate = false 
    Config = None },
  getConfigCmd()



open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
|> Program.withConsoleTrace
|> Program.run
