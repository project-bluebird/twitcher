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

let plotRectangularDisplay model =
  let sectorInfo = model.DisplayView.DisplayArea
  let minLon, minLat = sectorInfo.BottomLeft ||> CoordinateSystem.Mercator.xyToLonLat 
  let maxLon, maxLat = sectorInfo.TopRight ||> CoordinateSystem.Mercator.xyToLonLat 
  [
    let visualCoordinates =
        match model.SectorDisplay with        
        | TopDown -> 
            [ minLon, maxLat
              maxLon, maxLat
              maxLon, minLat
              minLon, minLat ]
            |> List.map (fun (x,y) -> 
                let x', y' = CoordinateSystem.rescaleSectorToView TopDown (x * 1.<longitude>,y * 1.<latitude>,0.<ft>) model.DisplayView
                string x' + "," + string y')
            |> String.concat " "
        | LateralNorthSouth ->
            [ minLon, sectorInfo.BottomAltitude
              minLon, sectorInfo.TopAltitude
              maxLon, sectorInfo.TopAltitude
              maxLon, sectorInfo.BottomAltitude ]
            |> List.map (fun (x,alt) -> 
                let x', alt' = CoordinateSystem.rescaleSectorToView LateralNorthSouth (x * 1.<longitude>,51. * 1.<latitude>,alt) model.DisplayView
                string x' + "," + string alt')
            |> String.concat " "
        | LateralEastWest ->
            [ minLat, sectorInfo.BottomAltitude
              minLat, sectorInfo.TopAltitude
              maxLat, sectorInfo.TopAltitude
              maxLat, sectorInfo.BottomAltitude ]
            |> List.map (fun (y,alt) -> 
                let y', alt' = CoordinateSystem.rescaleSectorToView LateralEastWest (0. * 1.<longitude>,y * 1.<latitude>,alt) model.DisplayView
                string y' + "," + string alt')
            |> String.concat " "            

    yield! 
      ([ polygon
        [
          Points visualCoordinates
          Style
            [ Fill "#e0e0e0"
              //Stroke "#4f4f4f"
              //StrokeWidth "2"
               ]
        ] []])

  ]

let waypointsView model =
  [
    match model.SectorInfo with
    | None -> yield! []
    | Some sector ->
        let waypoints = sector.Waypoints

        let toPlot =
          waypoints 
          |> List.ofArray
          |> List.collect (fun fix ->
            let x,y' = CoordinateSystem.rescaleSectorToView model.SectorDisplay (fix.Position.Coordinates.Longitude, fix.Position.Coordinates.Latitude, fix.Position.Altitude) model.DisplayView

            let y =
              match model.SectorDisplay with
              | TopDown -> y'
              | LateralEastWest | LateralNorthSouth -> 
                  let height = model.DisplayView.VisualisationViewSize |> snd
                  height * 0.95            
            
            [
              circle [
                Cx (string x)
                Cy (string y)
                R 3
                Style
                    [ Stroke "black"
                      StrokeWidth "0.5"
                      Fill "white" ]
              ] []
                      
              text [
                X (string (x + 7.))
                Y (string (y + 10.))
                Style [ Fill "black"; FontSize "12" ]
              ] [ str fix.Name ]
            ]
            )
        yield! toPlot 

  ]


let sectorOutlineView model dispatch =
  [
      // 1. Plot the sector outline  
      // TODO: plot the full outline - this plots only the rectangle

      yield! plotRectangularDisplay model 

      match model.SectorInfo with
      | None -> yield! []
      | Some outline ->
        
        let visualCoordinates = 
          match model.SectorDisplay with
          | TopDown -> 
              outline.Coordinates
              |> Array.map (fun coord ->
                  CoordinateSystem.rescaleSectorToView TopDown (coord.Longitude, coord.Latitude, 0.<ft>) model.DisplayView
                  )
              |> Array.map (fun (x,y) -> string x + "," + string y)
              |> String.concat " "

          | LateralNorthSouth ->
              let longitudes = outline.Coordinates |> Array.map (fun c -> c.Longitude)
              let minLon = longitudes |> Array.min 
              let maxLon = longitudes |> Array.max
              
              // Get vertical bounds of the sector space
              let minX, minY = CoordinateSystem.rescaleSectorToView LateralNorthSouth (minLon, outline.Coordinates.[0].Latitude, outline.BottomAltitude |> Conversions.Altitude.fl2ft) model.DisplayView
              let maxX, maxY = CoordinateSystem.rescaleSectorToView LateralNorthSouth (maxLon, outline.Coordinates.[0].Latitude, outline.TopAltitude |> Conversions.Altitude.fl2ft) model.DisplayView
              
              [ string minX + "," + string maxY
                string maxX + "," + string maxY
                string maxX + "," + string minY
                string minX + "," + string minY ]
              |> String.concat " "

          | LateralEastWest -> 
              let latitudes = outline.Coordinates |> Array.map (fun c -> c.Latitude)
              let minLat = latitudes |> Array.min
              let maxLat = latitudes |> Array.max
              
              // Get vertical bounds of the sector space
              let minX, minY = CoordinateSystem.rescaleSectorToView LateralEastWest (outline.Coordinates.[0].Longitude, minLat, outline.BottomAltitude |> Conversions.Altitude.fl2ft) model.DisplayView
              let maxX, maxY = CoordinateSystem.rescaleSectorToView LateralEastWest (outline.Coordinates.[0].Longitude, maxLat, outline.TopAltitude |> Conversions.Altitude.fl2ft) model.DisplayView
              
              [ string minX + "," + string maxY
                string maxX + "," + string maxY
                string maxX + "," + string minY
                string minX + "," + string minY ]
              |> String.concat " "


        yield! 
          ([ polygon
            [
              Points visualCoordinates
              Style
                [ Fill "white" ]
            ] []])


        if model.ShowWaypoints then
          yield! waypointsView model

  ]

let roundToHalf value =
  System.Math.Round(value * 2.0, System.MidpointRounding.ToEven) / 2.0

let areaLatitudesLongitudesView model dispatch =
  let x0, y0 = 
    model.DisplayView.DisplayArea.BottomLeft
    ||> CoordinateSystem.Mercator.xyToLonLat
  let x1, y1 = 
    model.DisplayView.DisplayArea.TopRight
    ||> CoordinateSystem.Mercator.xyToLonLat  
  let a0, a1 =
    float model.DisplayView.DisplayArea.BottomAltitude, float model.DisplayView.DisplayArea.TopAltitude

  let xTicks = 
    match model.SectorDisplay with
    | TopDown | LateralNorthSouth ->
      [roundToHalf x0 .. 0.1 .. roundToHalf x1] 
      |> List.map (fun x -> 
          let x', y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (x*1.<longitude>, y0*1.<latitude>, 0.<ft>) model.DisplayView
          string (System.Math.Round(x,2)) , x')
    | LateralEastWest ->
      [roundToHalf y0 .. 0.1 .. roundToHalf y1] 
      |> List.map (fun y -> 
          let x', y' = CoordinateSystem.rescaleSectorToView model.SectorDisplay (x0*1.<longitude>, y*1.<latitude>, 0.<ft>) model.DisplayView
          string (System.Math.Round(y,2)), x')  

  let yTicks = 
    match model.SectorDisplay with
    | TopDown ->
      [ roundToHalf y0 .. 0.1 .. roundToHalf y1] 
      |> List.map (fun y -> 
          let x, y' = CoordinateSystem.rescaleSectorToView model.SectorDisplay (x0*1.<longitude>, y*1.<latitude>, 0.<ft>) model.DisplayView
          string (System.Math.Round(y,2)), y')
    | LateralEastWest | LateralNorthSouth ->
      (match model.SectorInfo with
       | None -> [ a0 .. 1000. .. a1] 
       | Some outline ->
        [ outline.BottomAltitude .. 10<FL> .. outline.TopAltitude]
        |> List.map (Twitcher.Conversions.Altitude.fl2ft >> float) )
      |> List.map (fun a -> 
          let x, y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (x0*1.<longitude>, y0*1.<latitude>, a*1.<ft>) model.DisplayView
          "FL" + string (a/100.), y)     

  [
    yield! 
      xTicks 
      |> List.collect (fun (x, x') -> 
          [
          text [
              X (string x')
              Y (string 15)
              Style [ Fill "#636363"; FontSize "12" ]
            ] [ str (string x) ]
          text [
              X (string x')
              Y (string (snd model.DisplayView.VisualisationViewSize - 10.))
              Style [ Fill "#636363"; FontSize "12" ]
            ] [ str (string x) ]  
          line [
            X1 (string x')
            Y1 (string 0)
            X2 (string x')
            Y2 ((string (snd model.DisplayView.VisualisationViewSize)))
            Style [ Stroke "#b3b3b3"; StrokeWidth "0.3" ]
          ] []
          ])
      
    yield! 
      yTicks 
      |> List.collect (fun (y, y') -> 
          [
          text [
              X "0.5%"
              Y (string y')
              Style [ Fill "#636363"; FontSize "12" ]
            ] [ str (string y) ]
          // text [
          //     X ("98%")
          //     Y (string y')
          //     Style [ Fill "#636363"; FontSize "12" ]
          //   ] [ str (string y) ]  
          line [
            X1 (string 0)
            Y1 (string y')
            X2 ((string "100%"))
            Y2 (string y')
            Style [ Stroke "#b3b3b3"; StrokeWidth "0.3" ]
          ] []
          ])
    
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
        let x,y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.DisplayView

        circle [
          Cx (string x)
          Cy (string y)
          R (string (model.SeparationDistance.Value) + "px")
          Style
              [
                Fill "red"
                Opacity "0.25"
              ]
        ] []
    )

  // 3. Plot the actual aircraft as points
  yield!
    model.Positions
    |> List.collect (fun aircraft ->
        let position = aircraft.Position
        let x,y = CoordinateSystem.rescaleSectorToView model.SectorDisplay (position.Coordinates.Longitude, position.Coordinates.Latitude, position.Altitude) model.DisplayView
        let past =
          if (snd model.PositionHistory).ContainsKey aircraft.AircraftID then
            (snd model.PositionHistory).[aircraft.AircraftID]
            |> fun a -> a.[0..min a.Length 5]
            |> Array.map (fun pastPosition -> // TODO - precompute this?
              CoordinateSystem.rescaleSectorToView model.SectorDisplay (pastPosition.Coordinates.Longitude, pastPosition.Coordinates.Latitude, pastPosition.Altitude) model.DisplayView)
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
                R (if selected then "7" else if conflict then "8" else "3")
                Style
                    [ Stroke (if selected && not conflict then "turquoise" else if conflict then "orange" else "black")
                      StrokeWidth (if selected || conflict then "5" else "1")
                      Fill (if selected then (if conflict then "orange" else "black") else if conflict then "orange" else "grey") ]
                OnClick (fun _ -> dispatch (ViewAircraftDetails aircraft.AircraftID))
              ] []

          // Callsign
          yield
            text [
              X (string (x + 7.))
              Y (string (y + 18.))
              Style [ Fill "black"; FontSize "20" ]
            ] [ str aircraft.AircraftID ]

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
            SVGAttr.Height (model.DisplayView.VisualisationViewSize |> snd |> string)
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
                if CoordinateSystem.isInViewSector (pos.Position.Coordinates.Longitude, pos.Position.Coordinates.Latitude, pos.Position.Altitude) model.DisplayView then
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
        Level.item [ Level.Item.HasTextCentered ]
          [ div [] [
              Level.heading [ ]
                [ str "Score"  ]
              Level.title [ ]
                [ str (
                    sprintf "%.1f" model.Score
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

          Menu.Item.li
            [ Menu.Item.OnClick (fun _ -> dispatch MakeSimulatorStep)
              Menu.Item.Props []
             ] [
              Icon.icon [ ] [ Fa.i [Fa.Solid.StepForward][] ]
              str "Make step" ]
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
      Menu.list [ ]
        [
          Menu.Item.li [
            Menu.Item.OnClick (fun _ -> if model.ShowWaypoints then dispatch (ShowWaypoints false) else dispatch (ShowWaypoints true))
            ] [
              Icon.icon [ ] [ Fa.i [Fa.Solid.MapMarkerAlt][] ]
              Text.span [] [ str (if model.ShowWaypoints then "Hide waypoints" else "Show waypoints")]
            ]
          ]
      ]
  br []
  ]

let viewSimulatorControls model dispatch =
  Menu.menu [ ]
    [ Menu.label [ ] [ str "Simulator controls" ]
      Menu.list [ ]
        [ 
          Menu.Item.li
            [  ] [ 
            str "Load sector definition"
            input 
                [ 
                    Class "input"
                    Type "file"
                    OnInput (fun ev -> 
                        let file = ev.target?files?(0)

                        let reader = Browser.Dom.FileReader.Create()

                        reader.onload <- fun evt ->
                            dispatch (ReadSectorDefinition evt.target?result)

                        reader.onerror <- fun evt ->
                            dispatch ReadJsonErrorr

                        reader.readAsText(file)
                    ) 
                ] ]    

          Menu.Item.li
            [  ] [ 
            str "Load scenario"
            input 
                [ 
                    Class "input"
                    Type "file"
                    OnInput (fun ev -> 
                        let file = ev.target?files?(0)

                        let reader = Browser.Dom.FileReader.Create()

                        reader.onload <- fun evt ->
                            dispatch (ReadScenario evt.target?result)

                        reader.onerror <- fun evt ->
                            dispatch ReadJsonErrorr

                        reader.readAsText(file)
                    ) 
                ] ]    

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
                            viewDisplayMenu model dispatch
                            viewControlMenu model dispatch
                          ]

                      ]

                    viewSimulatorControls model dispatch

                    viewScore model dispatch

                    viewTimer model dispatch

                    Columns.columns [
                      Columns.IsCentered  ]
                      [

                        Column.column [ Column.Width(Screen.All, Column.Is4) ] [

                            viewPositionTable model dispatch
                        ]

                        Column.column [ Column.Width(Screen.All, Column.Is5)] [
                          viewAircraftDetails model dispatch
                        ]

                      ]

                    commandForm model dispatch

                   ] )]
