module Twitcher.View

open Twitcher.Domain
open Twitcher.Model

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Fulma.FontAwesome

open System.Collections.Generic

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
            [ img [ Style [ Width "7.65em"; Height "3.465em"; Margin "1em" ] // 511 × 231
                    Src "assets/Turing-logo.png" ] ] ] 

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
                |> List.filter (fun aircraft -> model.InConflict |> Array.contains aircraft.AircraftID)
                |> List.map (fun aircraft ->
                    let x,y = CoordinateSystem.rescaleCollege (aircraft.Position.Coordinates.Longitude, aircraft.Position.Coordinates.Latitude) model.SimulationViewSize

                    circle [ 
                      Cx (string x)
                      Cy (string y)
                      R (string (model.SeparationDistance.Value) + "px")
                      Style 
                          [
                            Fill "orange"
                            Opacity "0.25"
                          ]
                    ] []
                )              

              yield! 
                model.Positions  
                |> List.collect (fun aircraft ->
                    let x,y = CoordinateSystem.rescaleCollege (aircraft.Position.Coordinates.Longitude, aircraft.Position.Coordinates.Latitude) model.SimulationViewSize
                    let past = 
                      if (snd model.PositionHistory).ContainsKey aircraft.AircraftID then
                        (snd model.PositionHistory).[aircraft.AircraftID] 
                        |> Array.map (fun pastPosition -> // TODO - precompute this? 
                          CoordinateSystem.rescaleCollege (pastPosition.Coordinates.Longitude, pastPosition.Coordinates.Latitude) model.SimulationViewSize)
                      else [||]
                    let selected = model.ViewDetails = Some(aircraft.AircraftID)
                    let conflict = model.InConflict |> Array.contains aircraft.AircraftID

                    [
                      // plot current position and past path
                      if past.Length > 0 then 
                        let path = 
                          Array.append past [|x,y|]
                          |> fun a -> if selected then a else Array.skip (a.Length - 11) a
                          |> Array.map (fun (lon, lat) -> string lon + "," + string lat)
                          |> String.concat " "
                        yield
                          polyline [
                            Points path
                            Style [
                              Stroke "grey"
                              Opacity (if selected then "0.5" else "0.25")
                              StrokeWidth (if selected then "2" else "1.5")
                              Fill "none"
                            ]
                          ] []

                      if conflict then
                        yield
                          circle [ 
                            Cx (string x)
                            Cy (string y)
                            R (if selected then "5" else "3")
                            Style 
                                [ Stroke (if selected then "black" else "black")
                                  StrokeWidth (if selected then "1" else "1")
                                  Fill (if selected then "orange" else "grey") ]
                            OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
                          ] []      

                      else
                        yield
                          circle [ 
                            Cx (string x)
                            Cy (string y)
                            R (if selected then "5" else "3")
                            Style 
                                [ Stroke (if selected then "turquoise" else "black")
                                  StrokeWidth (if selected then "5" else "1")
                                  Fill (if selected then "black" else "grey") ]
                            OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
                          ] []
                    ]
                    )
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
                  [ td [] [ Heading.h6 [] [str "Course"] ]
                    td [] 
                      [ 
                          match info.Heading with 
                          | Some(x) -> yield (str (sprintf "%.1f" x + "°"))
                          | None -> yield (Button.a [ Button.IsLoading true; Button.IsOutlined; Button.IsText; Button.Size IsSmall ] [ str "Loading" ])
                           ]
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
                      [ str (sprintf "%.0f ft" info.Position.Altitude) ]
                    td [] [ 
                      Button.button 
                        [ Button.OnClick (fun _ -> dispatch (ShowChangeAltitudeForm info))
                          Button.Color IsPrimary
                          Button.IsOutlined ] 
                        [ Icon.faIcon [ ] [ Fa.icon Fa.I.ArrowsV ]
                          Text.span [] [str "Change" ]]]
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
                            Text.span [] [str "Change calib. air speed" ]]]                 
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

let viewPositionTable model dispatch =
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
              let className = [
                if model.InConflict |> Array.contains pos.AircraftID then
                    yield  "is-warning"
                  else
                    yield  ""
                match model.ViewDetails with
                  | Some(acid) when acid = pos.AircraftID -> 
                      yield  "is-bold "
                  | _ -> yield  ""
                if CoordinateSystem.isInViewCollege (pos.Position.Coordinates.Longitude, pos.Position.Coordinates.Latitude) model.SimulationViewSize then 
                   yield  ""
                else 
                   yield "is-greyed-out" ] |> String.concat " "

              tr [ OnClick (fun _ -> dispatch (ViewAircraftDetails pos.AircraftID)) :> IHTMLProp
                   ClassName className ] 
                  [ td [] [str pos.AircraftID]
                    td [] [str (sprintf "%.3f" pos.Position.Coordinates.Latitude)] 
                    td [] [str (sprintf "%.3f" pos.Position.Coordinates.Longitude)] 
                    td [] [str (sprintf "%.0f ft" (float pos.Position.Altitude)) ] ]
            ))
       ]  

let viewControlMenu model dispatch =
  Menu.menu [ ]
    [ Menu.label [ ] [ str "General controls" ]
      Menu.list [ ]
        [ Menu.Item.li 
            [ Menu.Item.OnClick (fun _ -> dispatch Observe) ] [ 
            Icon.faIcon [ ] [ Fa.icon Fa.I.Binoculars ]
            str "Run as observer" ]

          Menu.Item.li 
            [ Menu.Item.OnClick (fun _ -> dispatch (LoadScenario "/Users/egabasova/Projects/nats-birdhouse/scn_generator/scn_files/Assessment 1.json.scn")) ] [ 
            Icon.faIcon [ ] [ Fa.icon Fa.I.FileO ]
            str "Load test scenario" ]
          
          Menu.Item.li 
            [ Menu.Item.OnClick (fun _ -> dispatch ResetSimulator) ] [ 
            Icon.faIcon [ ] [ Fa.icon Fa.I.Times ]
            str "Reset simulator" ]                                  
        ]

      Menu.label [ ] [ str "Simulation controls" ]
      Menu.list [ ]
        [    
          Menu.Item.li 
            [ Menu.Item.OnClick (fun _ -> dispatch ResumeSimulation)
              (
                match model.State with
                 | ActiveSimulation Paused -> 
                    Menu.Item.Props []
                 | _ -> 
                    Menu.Item.Props [ ClassName "is-disabled" ])
             ] [ 
              Icon.faIcon [ ] [ Fa.icon Fa.I.Play ]
              str "Play/Resume" ]                                  
          
          Menu.Item.li 
            [ Menu.Item.OnClick (fun _ -> dispatch PauseSimulation)
              (
                match model.State with
                 | ActiveSimulation Playing -> 
                    Menu.Item.Props []
                 | _ -> 
                    Menu.Item.Props [ ClassName "is-disabled" ])
             ] [ 
              Icon.faIcon [ ] [ Fa.icon Fa.I.Pause ]
              Text.span [] [ str "Pause"]                                
          
             ]
          
          Menu.Item.li [
            Menu.Item.OnClick (fun _ -> dispatch ShowCreateAircraftForm)
            (
              match model.State with
               | ActiveSimulation _ -> 
                  Menu.Item.Props []
               | _ -> 
                  Menu.Item.Props [ ClassName "is-disabled" ])
            ] [ 
              Icon.faIcon [ ] [ Fa.icon Fa.I.Plane ]
              Text.span [] [ str "Create aircraft"]  
            ]       
          ]
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
                    Columns.columns [] 
                      [
                        Column.column [ Column.Width(Screen.All, Column.Is10)] 
                          [
                            viewSimulation model dispatch 
                          ]
                        Column.column [ Column.Width(Screen.All, Column.Is2)] 
                          [
                            viewControlMenu model dispatch
                          ]
                      ]
                    
                    
                    Columns.columns [ 
                      Columns.IsCentered  ]
                      [
                        Column.column [ Column.Width(Screen.All, Column.IsHalf) ] [
                            
                            viewPositionTable model dispatch
                        ]

                        Column.column [ Column.Width(Screen.All, Column.IsHalf)] [
                          viewAircraftDetails model dispatch
                        ]
                          
                      ]
                        
                    commandForm model dispatch
                    
                   ] )] 
