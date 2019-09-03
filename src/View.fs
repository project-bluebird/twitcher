module Twitcher.View

open Twitcher.Domain
open Twitcher.Model

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Fable.FontAwesome.Free

open System.Collections.Generic

open Elmish.React

open Fable.Import
open Fable.Core.JsInterop
open Thoth.Json


let basicNavbar model dispatch =
    Navbar.navbar [ ]
        [ Navbar.Brand.div [ ]
            [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                [ Icon.icon [ ] [
                  Fa.i [Fa.Solid.Binoculars] [] ]
                  Heading.p [ Heading.Is5 ] [ str "Twitcher" ]  ]]
          Navbar.End.div [ ]
            [ img [ Style [ Width "7.65em"; Height "3.465em"; Margin "1em" ] // 511 × 231
                    Src "assets/Turing-logo.png" ] ] ]

let topDownSimulationView model dispatch =
  [
  // 1. Plot the sector outline  
  yield!
    match model.Sector with
    | Some(points) ->
      let sectorCoordinates =
        points
        |> List.map (fun coord ->
          CoordinateSystem.rescaleCollege (coord.Longitude, coord.Latitude, 0.0<ft>) model.SimulationViewSize)
      
      let visualCoordinates = 
        match model.SectorDisplay with
        | TopDown -> 
            sectorCoordinates 
            |> List.map (fun (x,y,_) -> string x + "," + string y)
            |> String.concat " "
        | LateralNorthSouth ->
            let minX = sectorCoordinates |> List.minBy (fun (x,y,z) -> x)
            let maxX = sectorCoordinates |> List.maxBy (fun (x,y,z) -> x)
            
            // Get vertical bounds of the sector space
            ""

        | LateralEastWest -> 
            let minY = sectorCoordinates |> List.minBy (fun (x,y,z) -> y)
            let maxY = sectorCoordinates |> List.maxBy (fun (x,y,z) -> y)        
            ""


      [ polygon
          [
            Points visualCoordinates
            Style
              [ Fill "white" ]
          ] []]
    | None -> []


  // 2. Plot loss of separation distance circle around all aircraft that are "in conflict (lost separation)"
  yield!
    model.Positions
    |> List.filter (fun aircraft -> model.InConflict |> Array.contains aircraft.AircraftID)
    |> List.map (fun aircraft ->
        let position = aircraft.Position
        let x,y,z = CoordinateSystem.rescaleCollege (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.SimulationViewSize

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

  // 3. Plot the actual aircraft as points
  yield!
    model.Positions
    |> List.collect (fun aircraft ->
        let position = aircraft.Position
        let x,y,z = CoordinateSystem.rescaleCollege (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.SimulationViewSize
        let past =
          if (snd model.PositionHistory).ContainsKey aircraft.AircraftID then
            (snd model.PositionHistory).[aircraft.AircraftID]
            |> Array.map (fun pastPosition -> // TODO - precompute this?
              CoordinateSystem.rescaleCollege (pastPosition.Coordinates.Longitude, pastPosition.Coordinates.Latitude, pastPosition.Altitude) model.SimulationViewSize)
          else [||]
        let selected = model.ViewDetails = Some(aircraft.AircraftID)
        let conflict = model.InConflict |> Array.contains aircraft.AircraftID

        [
          // plot current position and past path
          if past.Length > 0 then
            let path =
              Array.append past [|x,y,z|]
              |> fun a -> if selected then a else Array.skip (a.Length - 11) a
              |> Array.map (fun (lon, lat, alt) -> string lon + "," + string lat)
              |> String.concat " "
            yield
              polyline [
                Points path
                Style [
                  Stroke "grey"
                  Opacity (if selected then "0.5" else "0.25")
                  StrokeWidth (if selected then "3" else "2")
                  Fill "none"
                ]
              ] []

          if conflict then
            yield
              circle [
                Cx (string x)
                Cy (string y)
                R (if selected then "7" else "5")
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
                R (if selected then "7" else "5")
                Style
                    [ Stroke (if selected then "turquoise" else "black")
                      StrokeWidth (if selected then "5" else "1")
                      Fill (if selected then "black" else "grey") ]
                OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
              ] []
        ]
        )
        ]

// let northSouthSimulationView model dispatch =
//   [
//     // Sector boundaries - the sector limits in the East and West
//     match model.Sector with
//     | Some(points) ->
//       let xCoordinates =
//         points
//         |> List.map (fun coord ->
//           let x,y = CoordinateSystem.rescaleCollege (coord.Longitude, coord.Latitude) model.SimulationViewSize
//           x )
//       let minX = xCoordinates |> List.min
//       let maxX = xCoordinates |> List.max
//       let points = 
//         // y is the altitude -> Rescale altitudes as well

//       yield!
//         [ polygon [
//             Points ""
//             Style [ Fill "white" ]
//         ] []]

//       //     string x + "," + string y )
//       //   |> String.concat " "

//       // [ polygon
//       //     [
//       //       Points coordinates
//       //       Style
//       //         [ Fill "white" ]
//       //     ] []]
//     | None -> 
//         yield! []

//   ]        

// TODO: implement lateral views
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
            (
              match model.SectorDisplay with
              | LateralNorthSouth -> []
              | LateralEastWest -> []
              | TopDown -> topDownSimulationView model dispatch

            )
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
    | Some(LoadScenarioForm f) ->
      [ ScenarioForm.view f (LoadScenarioMsg >> dispatch)]
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
            Icon.icon [ ] [ Fa.i [ Fa.Solid.Plane ] [] ]
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
                        [ Icon.icon [ ] [ Fa.i [Fa.Solid.LocationArrow] [] ]
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
                        [ Icon.icon [ ] [ Fa.i [Fa.Solid.ArrowsAltV] [] ]
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
                          [ Icon.icon [ ] [ Fa.i [Fa.Solid.TachometerAlt][] ]
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
                if CoordinateSystem.isInViewCollege (pos.Position.Coordinates.Longitude, pos.Position.Coordinates.Latitude, pos.Position.Altitude) model.SimulationViewSize then
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


let viewTimer model dispatch =
  Level.level [ ]
    [ Level.item [ Level.Item.HasTextCentered ]
        [ div [ ]
            [ Level.heading [ ]
                [ str "Simulation time" ]
              Level.title [ ]
                [ str (
                    sprintf "%02d:%02d:%02d"
                      model.SimulationTime.Hours
                      model.SimulationTime.Minutes
                      model.SimulationTime.Seconds
                )]
            ]
        ]
    ]

let viewControlMenu model dispatch =
  Menu.menu [ ]
    [ Menu.label [ ] [ str "General controls" ]
      Menu.list [ ]
        [ Menu.Item.li
            [ Menu.Item.OnClick (fun _ -> dispatch Observe) ] [
            Icon.icon [ ] [ Fa.i [Fa.Solid.Binoculars][] ]
            str "Run as observer" ]

          Menu.Item.li
            [ Menu.Item.OnClick (fun _ -> dispatch (LoadScenario "scenario/test-scenario.scn")) ] [
            //[ Menu.Item.OnClick (fun _ -> dispatch ShowLoadScenarioForm) ] [
            Icon.icon [ ] [ Fa.i [Fa.Solid.FileImport ][]]
            str "Load test scenario" ]

          Menu.Item.li
            [ Menu.Item.OnClick (fun _ -> dispatch ResetSimulator) ] [
            Icon.icon [ ] [ Fa.i [Fa.Solid.Times][] ]
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
              Icon.icon [ ] [ Fa.i [Fa.Solid.Play][] ]
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
              Icon.icon [ ] [ Fa.i [Fa.Solid.Pause][] ]
              Text.span [] [ str "Pause"]

             ]

          Menu.Item.li
            [ (match model.State with
                 | ActiveSimulation Playing | ActiveSimulation Observing | ReplaySimulation ->
                    Menu.Item.Props []
                 | _ ->
                    Menu.Item.Props [ ClassName "is-disabled" ])
             ] [
              Icon.icon [ ] [ Fa.i [Fa.Solid.TachometerAlt][] ]
              Text.span [] [ str "Simulator speed"]
              Field.div [ Field.HasAddons ]
                [
                  Control.div [] [
                    Button.button
                      [ Button.Size IsSmall;
                        Button.Color (if model.SimulationSpeed = 0.5 then IsLight else IsWhite)
                        Button.OnClick (fun _ -> dispatch (SetSimulationRateMultiplier 0.5)) ]
                      [ str "0.5×"] ]
                  Control.div [] [
                    Button.button
                      [ Button.Size IsSmall;
                        Button.Color (if model.SimulationSpeed = 1.0 then IsLight else IsWhite)
                        Button.OnClick (fun _ -> dispatch (SetSimulationRateMultiplier 1.0)) ]
                      [ str "1×"] ]
                  Control.div [] [
                    Button.button
                      [ Button.Size IsSmall;
                        Button.Color (if model.SimulationSpeed = 2.0 then IsLight else IsWhite)
                        Button.OnClick (fun _ -> dispatch (SetSimulationRateMultiplier 2.0)) ]
                      [ str "2×"] ]
                  Control.div [] [
                    Button.button
                      [ Button.Size IsSmall;
                        Button.Color (if model.SimulationSpeed = 5.0 then IsLight else IsWhite)
                        Button.OnClick (fun _ -> dispatch (SetSimulationRateMultiplier 5.0)) ]
                      [ str "5×"] ]
                  Control.div [] [
                    Button.button
                      [ Button.Size IsSmall;
                        Button.Color (if model.SimulationSpeed = 10. then IsLight else IsWhite)
                        Button.OnClick (fun _ -> dispatch (SetSimulationRateMultiplier 10.)) ]
                      [ str "10×"] ]
                ]

             ]
        ]

      Menu.label [ ] [ str "Aircraft controls" ]
      Menu.list [ ]
        [
          Menu.Item.li [
            Menu.Item.OnClick (fun _ -> dispatch ShowCreateAircraftForm)
            (
              match model.State with
               | ActiveSimulation _ ->
                  Menu.Item.Props []
               | _ ->
                  Menu.Item.Props [ ClassName "is-disabled" ])
            ] [
              Icon.icon [ ] [ Fa.i [Fa.Solid.Plane][] ]
              Text.span [] [ str "Create aircraft"]
            ]
          ]
      ]

let viewDisplayMenu model dispatch =
  div [] [
  hr []
  Menu.menu [ ]
    [ Menu.label [ ] [ str "Display controls" ]
      Field.div [ Field.HasAddons ]
        [
          Control.div [] [
            Button.button
              [ Button.Size IsMedium;
                Button.Color (if model.SectorDisplay = TopDown then IsLight else IsWhite)
                Button.OnClick (fun _ -> dispatch (ChangeDisplay SectorDisplay.TopDown)) ]
              [ Icon.icon [ ] [ Fa.i [Fa.Solid.Eye ][] ]] 
            Button.button
              [ Button.Size IsMedium;
                Button.Color (if model.SectorDisplay = LateralEastWest then IsLight else IsWhite)
                Button.OnClick (fun _ -> dispatch (ChangeDisplay SectorDisplay.LateralEastWest)) ]
              [ Icon.icon [ ] [ Fa.i [Fa.Solid.ArrowsAltH ][] ]] 
            Button.button
              [ Button.Size IsMedium;
                Button.Color (if model.SectorDisplay = LateralNorthSouth then IsLight else IsWhite)
                Button.OnClick (fun _ -> dispatch (ChangeDisplay SectorDisplay.LateralNorthSouth)) ]
              [ Icon.icon [ ] [ Fa.i [Fa.Solid.ArrowsAltV ][] ]] 
          ]
        ]
    ]]

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
                            viewDisplayMenu model dispatch
                          ]
                      ]

                    viewTimer model dispatch

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
