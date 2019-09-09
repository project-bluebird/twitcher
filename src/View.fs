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

let plotRectangularSector model (sectorInfo: SectorInfo) =
  [
    let visualCoordinates =
        match model.SectorDisplay with        
        | TopDown -> 
            [ sectorInfo.min_lon, sectorInfo.max_lat
              sectorInfo.max_lon, sectorInfo.max_lat
              sectorInfo.max_lon, sectorInfo.min_lat
              sectorInfo.min_lon, sectorInfo.min_lat ]
            |> List.map (fun (x,y) -> 
                // TODO: projection
                let x', y' = CoordinateSystem.rescaleSectorToView TopDown (x * 1.<longitude>,y * 1.<latitude>,0.<ft>) model.SectorView
                string x' + "," + string y')
            |> String.concat " "
        | LateralNorthSouth ->
            [ sectorInfo.min_lon, sectorInfo.min_alt
              sectorInfo.min_lon, sectorInfo.max_alt
              sectorInfo.max_lon, sectorInfo.max_alt
              sectorInfo.max_lon, sectorInfo.min_alt ]
            |> List.map (fun (x,alt) -> 
                // TODO: projection
                let x', alt' = CoordinateSystem.rescaleSectorToView LateralNorthSouth (x * 1.<longitude>,51. * 1.<latitude>,alt * 1.<ft>) model.SectorView
                string x' + "," + string alt')
            |> String.concat " "
        | LateralEastWest ->
            [ sectorInfo.min_lat, sectorInfo.min_alt
              sectorInfo.min_lat, sectorInfo.max_alt
              sectorInfo.max_lat, sectorInfo.max_alt
              sectorInfo.max_lat, sectorInfo.min_alt ]
            |> List.map (fun (y,alt) -> 
                // TODO: projection
                let y', alt' = CoordinateSystem.rescaleSectorToView LateralEastWest (0. * 1.<longitude>,y * 1.<latitude>,alt * 1.<ft>) model.SectorView
                string y' + "," + string alt')
            |> String.concat " "            

    yield! 
      ([ polygon
        [
          Points visualCoordinates
          Style
            [ Fill "#F6F6F6"
              Stroke "lightgrey"
               ]
        ] []])

  ]

let sectorOutlineView model dispatch =
  [
      // 1. Plot the sector outline  
    match model.Sector with
    | Some(sectors) -> 
        let sectorsToPlot = 
          sectors.sectors
          |> List.collect (plotRectangularSector model)
        
        yield! sectorsToPlot

      // let points = sectors.sectors.[0]
      // let sectorCoordinates =

      //   points
      //   |> Array.map (fun coord ->
      //     CoordinateSystem.rescaleSectorToView (coord.Longitude, coord.Latitude, 0.0<ft>) model.SimulationViewSize)
      
      // let visualCoordinates = 
      //   match model.SectorDisplay with
      //   | TopDown -> 
      //       sectorCoordinates 
      //       |> List.map (fun (x,y,_) -> string x + "," + string y)
      //       |> String.concat " "

      //   | LateralNorthSouth ->
      //       let minX = sectorCoordinates |> List.map (fun (x,y,z) -> x) |> List.min
      //       let maxX = sectorCoordinates |> List.map (fun (x,y,z) -> x) |> List.max
            
      //       // Get vertical bounds of the sector space
      //       let _, _, minAlt = CoordinateSystem.rescaleSectorToView (points.[0].Longitude, points.[0].Latitude, CoordinateSystem.minAltitude) model.SimulationViewSize
      //       let _, _, maxAlt = CoordinateSystem.rescaleSectorToView (points.[0].Longitude, points.[0].Latitude, CoordinateSystem.maxAltitude) model.SimulationViewSize
            
      //       [ string minX + "," + string maxAlt
      //         string maxX + "," + string maxAlt
      //         string maxX + "," + string minAlt
      //         string minX + "," + string minAlt ]
      //       |> String.concat " "

      //   | LateralEastWest -> 
      //       // TODO rescale latitude correctly to "x" in svg element, right now it's still rescaled to y
      //       let minY = sectorCoordinates |> List.map (fun (x,y,z) -> x) |> List.min
      //       let maxY = sectorCoordinates |> List.map (fun (x,y,z) -> x) |> List.max
            
      //       // Get vertical bounds of the sector space
      //       let _, _, minAlt = CoordinateSystem.rescaleSectorToView (points.[0].Longitude, points.[0].Latitude, CoordinateSystem.minAltitude) model.SimulationViewSize
      //       let _, _, maxAlt = CoordinateSystem.rescaleSectorToView (points.[0].Longitude, points.[0].Latitude, CoordinateSystem.maxAltitude) model.SimulationViewSize
            
      //       [ string minY + "," + string maxAlt
      //         string maxY + "," + string maxAlt
      //         string maxY + "," + string minAlt
      //         string minY + "," + string minAlt ]
      //       |> String.concat " "

      // yield! 
      //   ([ polygon
      //     [
      //       Points visualCoordinates
      //       Style
      //         [ Fill "white" ]
      //     ] []])

    | None -> yield! []
  ]

let areaLatitudesLongitudesView model dispatch =
  [
    yield! [
      // TODO

    ]
  ]

let sectorView model dispatch =
  [
    yield! sectorOutlineView model dispatch
    yield! areaLatitudesLongitudesView model dispatch
  ]  

let simulationView model dispatch =
  [
  // 2. Plot loss of separation distance circle around all aircraft that are "in conflict (lost separation)"
  yield!
    model.Positions
    |> List.filter (fun aircraft -> model.InConflict |> Array.contains aircraft.AircraftID)
    |> List.map (fun aircraft ->
        let position = aircraft.Position
        let x,y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.SectorView

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
        let x,y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.SectorView
        let past =
          if (snd model.PositionHistory).ContainsKey aircraft.AircraftID then
            (snd model.PositionHistory).[aircraft.AircraftID]
            |> Array.map (fun pastPosition -> // TODO - precompute this?
              CoordinateSystem.rescaleSectorToView model.SectorDisplay (pastPosition.Coordinates.Longitude, pastPosition.Coordinates.Latitude, pastPosition.Altitude) model.SectorView)
          else [||]
        let selected = model.ViewDetails = Some(aircraft.AircraftID)
        let conflict = model.InConflict |> Array.contains aircraft.AircraftID

        [
          // plot current position and past path
          if past.Length > 0 then
            let path =
              Array.append past [|x,y|]
              |> fun a -> if selected then a else Array.skip (a.Length - 11) a
              |> Array.map (fun (x,y) -> string x + "," + string y)
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

          yield
              circle [
                Cx (string x)
                Cy (string y)
                R (if selected then "7" else "3")
                Style
                    [ Stroke (if selected && not conflict then "turquoise" else "black")
                      StrokeWidth (if selected || conflict then "5" else "1")
                      Fill (if selected then (if conflict then "orange" else "black") else "grey") ]
                OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
              ] []

        ]
        )
        ]


let viewSimulation model dispatch =
  Columns.columns [ Columns.IsCentered  ]
    [
      Column.column [ Column.Width(Screen.All, Column.IsFull) ] [
        div [ 
          //ClassName "svg-box" 
          ] [
          svg [
            //ClassName "svg-box-content"
            Style [ BackgroundColor "#e0e0e0" ]
            Id "simulation-viewer"
            SVGAttr.Width "100%"
            SVGAttr.Height (model.SectorView.VisualisationViewSize |> snd |> string)
            ]
            [
              yield! sectorView model dispatch
              yield! simulationView model dispatch
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
                if CoordinateSystem.isInViewSector (pos.Position.Coordinates.Longitude, pos.Position.Coordinates.Latitude, pos.Position.Altitude) model.SectorView then
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

let viewScore model dispatch =
  Level.level [ ]
    [ 
      for team in 0..model.TeamCount-1 ->
        Level.item [ Level.Item.HasTextCentered ]
          [ div [] [
              Level.heading [ ]
                [ str ("Team " + string (team + 1))  ]
              Level.title [ ]
                [ str (
                    string model.TeamScores.[team]
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
      ]
  br []
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
                        Column.column [ Column.Width(Screen.All, Column.IsFull)]
                          [
                            viewSimulation model dispatch
                          ]
                      ]

                    viewScore model dispatch

                    viewTimer model dispatch

                    Columns.columns [
                      Columns.IsCentered  ]
                      [
                        Column.column [ Column.Width(Screen.All, Column.Is2)]
                          [
                            viewDisplayMenu model dispatch
                            viewControlMenu model dispatch
                          ]

                        Column.column [ Column.Width(Screen.All, Column.Is4) ] [

                            viewPositionTable model dispatch
                        ]

                        Column.column [ Column.Width(Screen.All, Column.Is5)] [
                          viewAircraftDetails model dispatch
                        ]

                      ]

                    commandForm model dispatch

                   ] )]
