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

[<Literal>]
let UrlPosition = "http://localhost:5001/api/v1/pos"
[<Literal>]
let UrlReset = "http://localhost:5001/api/v1/ic"

type PositionRequest = {
  acid : string
}

// MODEL

type Model = {
  Animate : bool
  State : Coordinates []
}

type Msg =
| GetPosition
| FetchedPosition of PositionInfo[]
| FetchError of exn
| Step of unit
| ErrorMessage of exn
| StartAnimation
| StopAnimation


let getSimulationState () =
  promise {
      let url = UrlPosition
      let body = Encode.Auto.toString(0, { acid = "ALL" })
      Browser.console.log(body)
      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body ]

      let! res = Fetch.fetch url props
      let! txt = res.text()
      return Decode.Auto.unsafeFromString<PositionInfo[]> txt
  }

let getSimulationStateCmd () =
  Cmd.ofPromise getSimulationState () FetchedPosition FetchError

let delayMsg _ =
  promise {
    do! Promise.sleep 1000
    return ()
  }


let init() =
  { State = [||]
    Animate = false },
  Cmd.none

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | GetPosition ->
         model,
         getSimulationStateCmd()

    | FetchedPosition positionInfo ->
        let coordinates = 
          positionInfo
          |> Array.map (fun pos -> 
              let (x,y) = 
                lonlatToMercator pos.lon pos.lat
                |> rescaleTest 
              { X = x; Y = y; Altitude = pos.alt })
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
           getSimulationStateCmd()
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
    Hero.hero [ Hero.IsFullHeight ]
      [
        basicNavbar ()

        Hero.body [ ]
          [ Container.container [ ]
              [ Columns.columns [  ]
                  [
                    div []
                      [
                        Table.table [ Table.IsHoverable ]
                            [ thead [ ]
                                [ tr [ ]
                                    [ th [ ] [ str "x" ]
                                      th [ ] [ str "y" ]
                                      th [ ] [ str "Altitude" ] ] ]
                              tbody [ ]
                                (model.State 
                                |> List.ofArray
                                |> List.map (fun coord -> 
                                    tr [] [ td [] [str (sprintf "%.1f" coord.X)] 
                                            td [] [str (sprintf "%.1f" coord.Y)] 
                                            td [] [str (string coord.Altitude)] ]
                                ))
                             ]

                        Button.button [
                          Button.OnClick (fun _ -> dispatch GetPosition )
                          Button.Color IsInfo ]
                          [ str "Fetch position" ]

                        Container.container [] [

                          Button.button [
                            Button.OnClick (fun _ -> dispatch StartAnimation)
                            ] [ str "Start"]
                          Button.button [
                            Button.OnClick (fun _ -> dispatch StopAnimation)
                            ] [ str "Stop"]
                        ]
                      ]
                   ] ] ] ]

open Elmish.Debug
open Elmish.HMR  // hot module reloading

Program.mkProgram init update view
#if DEBUG
|> Program.withHMR
#endif
|> Program.withReactUnoptimized "elmish-app"
|> Program.withConsoleTrace
|> Program.run
