module Twitcher.View

open Twitcher.Domain
open Twitcher.Model

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




let basicNavbar model dispatch =
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

let viewSimulation model dispatch =
  Columns.columns [ Columns.IsCentered  ]
    [
      Column.column [ Column.Width(Screen.All, Column.IsFull) ] [
        div [ ClassName "svg-box" ] [
          svg [
            ClassName "svg-box-content"
            Style [ BackgroundColor "#e0e0e0" ]
            Id "simulation-viewer"
            ] 
            [
              yield! 
                match model.Sector with
                | Some(points) -> 
                  let coordinates = 
                    points 
                    |> List.map (fun (lat,lon) -> 
                      let x,y = CoordinateSystem.rescaleCollege (lon, lat) model.SimulationViewSize
                      string x + "," + string y )
                    |> String.concat " "

                  [ polygon 
                      [
                        Points coordinates                       
                        Style 
                          [ Fill "white" ]
                      ] []]
                | None -> []                    

              yield! 
                model.Positions  
                |> List.map (fun coord ->
                    let x,y = CoordinateSystem.rescaleCollege (coord.Longitude, coord.Latitude) model.SimulationViewSize
                    circle [ 
                      Cx (string x)
                      Cy (string y)
                      R "3"
                      Style 
                        [ Stroke "black"
                          StrokeWidth "1"
                          Fill "grey" ]
                      OnClick (fun _ -> Browser.console.log(coord.AircraftID))
                    ] [])
            ]
        ]
      ]
    ]

  
let commandForm model dispatch =
  Container.container []
   (match model.FormModel with
    | Some(CreateAircraftForm f) ->
      [ AircraftForm.view f (CreateAircraftMsg >> dispatch) ]
    | Some(ChangeAltitudeForm f) ->
      [ AltitudeForm.view f (ChangeAltitudeMsg >> dispatch) ]
    | _ ->
      [])         
 
                
let view model dispatch =
    Hero.hero [  ]
      [
        basicNavbar model dispatch

        Container.container [ ]
             (match model.State with 
               | NotConnected ->
                   [ Button.button [ Button.OnClick (fun _ -> dispatch Init); Button.IsFullWidth ] [ str "Start" ] ]
               | ConnectionFailed ->
                   [ Heading.p [ Heading.Is3 ] [ str "Connection failed" ] ]
               | _ ->
                  [ 
                    viewSimulation model dispatch 
                    
                    Columns.columns [ 
                      Columns.IsCentered  ]
                      [
                        Column.column [ Column.Width(Screen.All, Column.IsTwoThirds) ] [
                            Table.table [ Table.IsHoverable;  ]
                                [ thead [ ]
                                    [ tr [ ]
                                        [ th [ ] [ str "Aircraft ID" ]
                                          th [ ] [ str "Latitude" ]
                                          th [ ] [ str "Longitude" ]
                                          th [ ] [ str "Altitude" ] ] ]
                                  tbody [ ]
                                    (model.Positions 
                                    |> List.map (fun pos -> 
                                        tr [] [ td [] [str pos.AircraftID]
                                                td [] [str (sprintf "%.3f" pos.Latitude)] 
                                                td [] [str (sprintf "%.3f" pos.Longitude)] 
                                                td [] [str (string pos.Altitude)] ]
                                    ))
                                 ]
                        ]

                        Column.column [ Column.Width(Screen.All, Column.Is2)] [


                          Button.button [
                            Button.OnClick (fun _ -> dispatch (LoadScenario "/Users/egabasova/Projects/nats-birdhouse/scn_generator/scn_files/Assessment 1.json.scn"))
                            ] [ str "Load test scenario"]  
                          
                          Button.button [
                            Button.OnClick (fun _ -> dispatch ResetSimulator)
                            ] [ 
                              Icon.faIcon [ ] [ Fa.icon Fa.I.Times ]
                              Text.span [] [ str "Reset simulator"]  
                            ]

                          Button.button [
                            Button.OnClick (fun _ -> dispatch ResumeSimulation)
                            Button.Disabled (
                                match model.State with
                                 | ActiveSimulation Paused -> false
                                 | _ -> true)
                            ] [ 
                              Icon.faIcon [ ] [ Fa.icon Fa.I.Play ]
                              Text.span [] [ str "Play/Resume"]  
                            ]    

                          Button.button [
                            Button.OnClick (fun _ -> dispatch PauseSimulation)
                            Button.Disabled (
                                match model.State with
                                 | ActiveSimulation Playing -> false
                                 | _ -> true)
                            ] [ 
                              Icon.faIcon [ ] [ Fa.icon Fa.I.Pause ]
                              Text.span [] [ str "Pause"]  
                            ]  
                        ]
                          
                      ]

                    Button.button [
                      Button.OnClick (fun _ -> dispatch ShowCreateAircraftForm)
                      Button.Disabled (
                          match model.State with
                           | ActiveSimulation _ -> false
                           | _ -> true)
                      ] [ 
                        Icon.faIcon [ ] [ Fa.icon Fa.I.Plane ]
                        Text.span [] [ str "Create aircraft"]  
                      ]      
                        
                    commandForm model dispatch
  
                   ] )] 
