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
                    |> List.map (fun coord -> 
                      let x,y = CoordinateSystem.rescaleCollege (coord.Longitude, coord.Latitude) model.SimulationViewSize
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
                |> List.map (fun aircraft ->
                    let x,y = CoordinateSystem.rescaleCollege (aircraft.Position.Coordinates.Longitude, aircraft.Position.Coordinates.Latitude) model.SimulationViewSize
                    circle [ 
                      Cx (string x)
                      Cy (string y)
                      R "3"
                      Style 
                        [ Stroke "black"
                          StrokeWidth "1"
                          Fill "grey" ]
                      OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
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
    | Some(ChangeSpeedForm f) ->
      [ SpeedForm.view f (ChangeSpeedMsg >> dispatch)]
    | Some(ChangeHeadingForm f) ->
      [ HeadingForm.view f (ChangeHeadingMsg >> dispatch)]
    | None -> []
    )         
 

let viewAircraftDetails model dispatch =
  div [] [
    match model.ViewDetails with
    | Some(aircraft) ->
      let info = model.Positions |> List.find (fun ac -> ac.AircraftID = aircraft)

      yield! [
        Message.message [ Message.Color IsPrimary ] [
          Message.header [] [
            Icon.faIcon [ ] [ Fa.icon Fa.I.Plane ]
            str info.AircraftID
            Delete.delete [ Delete.OnClick (fun _ -> dispatch CloseAircraftDetails) ] []
          ]
          Message.body [] [

            Table.table [ Table.Props [ClassName "table-no-border"] ] 
              [
                tr []
                  [ td [] [ Heading.h6 [] [str "Longitude"] ]
                    td [] 
                      [ str (sprintf "%.3f" info.Position.Coordinates.Longitude) ]
                    td [] []]    
                tr []
                  [ td [] [ Heading.h6 [] [str "Latitude"] ]
                    td [] 
                      [ str (sprintf "%.3f" info.Position.Coordinates.Latitude) ]
                    td [] []]                                      
                tr []
                  [ td [] [ Heading.h6 [] [str "Heading"] ]
                    td [] 
                      [ str (
                          match info.Heading with 
                          | Some(x) -> sprintf "%.1f" x + "°"
                          | None -> "unknown") ]
                    td [] [ 
                      Button.button 
                        [ Button.OnClick (fun _ -> dispatch (ShowChangeHeadingForm info))
                          Button.Color IsPrimary
                          Button.IsOutlined ] 
                        [ Icon.faIcon [ ] [ Fa.icon Fa.I.LocationArrow ]
                          Text.span [] [ str "Change heading" ]]]]
                tr []
                  [ td [] [ Heading.h6 [] [str "Altitude"] ]
                    td [] 
                      [ str (
                          match info.Position.Altitude with 
                          | Altitude(x) -> sprintf "%.0f" x + " feet"
                          | FlightLevel(x) -> "FL" + string x) ]
                    td [] [ 
                      Button.button 
                        [ Button.OnClick (fun _ -> dispatch (ShowChangeAltitudeForm info))
                          Button.Color IsPrimary
                          Button.IsOutlined ] 
                        [ Icon.faIcon [ ] [ Fa.icon Fa.I.ArrowsV ]
                          Text.span [] [str "Change altitude" ]]]
                  ]
                tr []
                  [ td [] [ Heading.h6 [] [str "Ground speed"] ]
                    td [] 
                      [ str (
                          match info.GroundSpeed with 
                          | Some(s) -> sprintf "%.0f" s + " knots"
                          | None  -> "unknown" ) ]
                    td [] [
                      Button.button 
                          [ Button.OnClick (fun _ -> dispatch (ShowChangeSpeedForm info)) 
                            Button.Color IsPrimary
                            Button.IsOutlined ] 
                          [ Icon.faIcon [ ] [ Fa.icon Fa.I.Tachometer ]
                            Text.span [] [str "Change calibrated air speed" ]]]                 
                    ]
                tr []
                  [ td [] [ Heading.h6 [] [str "Vertical speed"] ]
                    td [] 
                      [ str (
                          match info.VerticalSpeed with 
                          | Some(s) -> sprintf "%.1f" s + " ft/min"
                          | None -> "unknown") ]
                    td [] []]                    
              ]

            
          ]
        ]
      ]
    | None ->
      yield! []
  ]
                
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
                        Column.column [ Column.Width(Screen.All, Column.IsHalf) ] [
                            viewAircraftDetails model dispatch

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
                                                td [] [str (sprintf "%.3f" pos.Position.Coordinates.Latitude)] 
                                                td [] [str (sprintf "%.3f" pos.Position.Coordinates.Longitude)] 
                                                td [] [str (sprintf "%.0f" (match pos.Position.Altitude with | Altitude a -> float a | FlightLevel fl -> (float fl)/1000.)  + " ft" )] ]
                                    ))
                                 ]
                        ]

                        Column.column [ Column.Width(Screen.All, Column.Is2)] [


                          Button.button [
                            Button.OnClick (fun _ -> dispatch (LoadScenario "/Users/egabasova/Projects/nats-birdhouse/scn_generator/scn_files/Assessment 1.json.scn"))
                            ] [ Icon.faIcon [ ] [ Fa.icon Fa.I.FileO ]
                                Text.span [] [ str "Load test scenario"]  ]
                          
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
                        ]
                          
                      ]

    
                        
                    commandForm model dispatch
  
                   ] )] 
