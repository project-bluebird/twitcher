module Twitcher.View

open Twitcher.Domain
open Twitcher.CoordinateSystem

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



// MODEL

type Model = {
  Animate : bool
  State : Coordinates list
  Config : Configuration option
}

type Msg =
| Config of Configuration
| GetPosition
| FetchedPosition of PositionInfo[]
| FetchError of exn
| Step of unit
| ErrorMessage of exn
| StartAnimation
| StopAnimation

let getSimulationStateCmd config =
  Cmd.ofPromise Commands.getSimulationState (config) FetchedPosition FetchError



let getConfigCmd () = 
  Cmd.ofPromise Commands.getConfig () Config FetchError

let delayMsg _ =
  promise {
    do! Promise.sleep 1000
    return ()
  }


let init() =
  { State = []
    Animate = false 
    Config = None },
  getConfigCmd()

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Config config ->
       { model with Config = Some config }, Cmd.none
       
    | GetPosition ->
        match model.Config with
        | None ->
            Browser.console.log("No configuration found")
            model, Cmd.none
        | Some config ->
            model,
            getSimulationStateCmd config

    | FetchedPosition positionInfo ->
        let coordinates = 
          positionInfo
          |> Array.map (fun pos -> 
              let (x,y) = 
                lonlatToMercator pos.lon pos.lat
                |> rescaleTest 
              { X = x; Y = y; Altitude = pos.alt })
          |> List.ofArray
        { model with State = coordinates } ,
        Cmd.none

    | FetchError exn | ErrorMessage exn ->
        Browser.console.error(exn)
        model,
        Cmd.none

    | Step _ ->
        if model.Animate then
          model,
          Cmd.batch [
           getSimulationStateCmd model.Config.Value
           Cmd.ofPromise delayMsg () Step ErrorMessage
          ]
        else
          model,
          Cmd.none

    | StartAnimation ->
        { model with Animate = true }, Cmd.ofMsg (Step())

    | StopAnimation ->
        { model with Animate = false }, Cmd.none

let private basicNavbar () =
    Navbar.navbar [ ]
        [ Navbar.Brand.div [ ]
            [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                [ Icon.faIcon [ ] [
                  Fa.icon Fa.I.Binoculars ]
                  Heading.p [ Heading.Is5 ] [ str "Twitcher" ]  ]]
          Navbar.Item.div [ Navbar.Item.HasDropdown
                            Navbar.Item.IsHoverable ]
            [ Navbar.Link.a [ ]
                [ str "Docs" ]
              Navbar.Dropdown.div [ ]
                [ Navbar.Item.a [ ]
                    [ str "Overwiew" ]
                  Navbar.Item.a [ ]
                    [ str "Something" ]
                  Navbar.divider [ ] [ ]
                  Navbar.Item.a [ ]
                    [ str "Something else" ] ] ]
          Navbar.End.div [ ]
            [ Navbar.Item.div [ ]
                [ str "The Alan Turing Institute" ] ] ]

let private view model dispatch =
    Hero.hero [  ]
      [
        basicNavbar ()

        Hero.body [ ]
          [ Container.container [ ]
              [ 
                Columns.columns [ Columns.IsCentered ] [
                      svg [
                        Props.Height "540"
                        Props.Width "1080"
                        Style [ BackgroundColor "#f9f9f9" ]
                      ] 
                        (model.State
                         |> List.map (fun coord ->
                            circle [ 
                              Cx (string coord.X)
                              Cy (string coord.Y)
                              R "3"
                              Style 
                                [ Stroke "black"
                                  StrokeWidth "1"
                                  Fill "grey" ]
                            ] []))
                    ]

                Columns.columns [ 
                  Columns.IsCentered  ]
                  [
                    Column.column [ Column.Width(Screen.All, Column.IsHalf) ] [
                        Table.table [ Table.IsHoverable; Table.IsFullWidth ]
                            [ thead [ ]
                                [ tr [ ]
                                    [ th [ ] [ str "x" ]
                                      th [ ] [ str "y" ]
                                      th [ ] [ str "Altitude" ] ] ]
                              tbody [ ]
                                (model.State 
                                |> List.map (fun coord -> 
                                    tr [] [ td [] [str (sprintf "%.1f" coord.X)] 
                                            td [] [str (sprintf "%.1f" coord.Y)] 
                                            td [] [str (string coord.Altitude)] ]
                                ))
                             ]
                    ]

                    Column.column [ Column.Width(Screen.All, Column.IsNarrow)] [
                        Button.button [
                          Button.OnClick (fun _ -> dispatch GetPosition )
                          Button.Color IsInfo
                          Button.IsFullWidth ]
                          [ str "Fetch position" ]
                    ]
                        
                    Column.column [ 
                      Column.Width(Screen.All, Column.IsNarrow) ] [
                        Button.button [
                          Button.OnClick (fun _ -> dispatch StartAnimation)
                          ] [ str "Start"]

                        Button.button [
                          Button.OnClick (fun _ -> dispatch StopAnimation)
                          ] [ str "Stop"]
                      
                    ]
                  ]
                   ] ] ] 

open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
|> Program.withConsoleTrace
|> Program.run
