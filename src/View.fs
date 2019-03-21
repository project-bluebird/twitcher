module Twitcher.View

open Twitcher.Domain

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

let basicNavbar () =
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

                
let view model dispatch =
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
                          Button.OnClick (fun _ -> dispatch GetAllPositions )
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

                    Button.button [
                          Button.OnClick (fun _ -> dispatch (LoadScenario "/Users/egabasova/Projects/nats-birdhouse/scn_generator/scn_files/Assessment 1.json.scn"))
                          ] [ str "Test"]
                  ]
                  
                   ] ] ] 
