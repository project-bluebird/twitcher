module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

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

type PositionInfo = {
    _validTo: string
    alt: int
    gs: float
    lat: float
    lon: float
    vs: float
}

// MODEL

type Model = {
  Animate : bool
  State : string
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
  { State = "Start"
    Animate = false },
  Cmd.none

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | GetPosition ->
         model,
         getSimulationStateCmd()
    | FetchedPosition positionInfo -> 
        { model with State = string positionInfo.[0].lat } ,
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


// VIEW (rendered with React)

let view (model:Model) dispatch =

  div []
      [ 
        button [ OnClick (fun _ -> dispatch GetPosition ) ] [ str "Update position" ]
        div [] [ str model.State ]
        button [ OnClick (fun _ -> dispatch StartAnimation) ] [ str "Start"]
        button [ OnClick (fun _ -> dispatch StopAnimation) ] [ str "Stop"]
      ]

// App
Program.mkProgram init update view
//|> Program.withSubscription timer
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
