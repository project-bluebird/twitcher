
module Twitcher.ScenarioForm

open Twitcher.Domain
open Twitcher.Form

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

type FormModel = 
  { Path : string
    CheckFields : bool
   }

type Msg = 
  | ChangePath of string
  | SubmitForm
  | Cancel
  | Error

type ExternalMsg =
    | NoOp
    | Submit of string
    | Cancel

let init() =
  { Path = ""; CheckFields = false },
  Cmd.none    

let update msg model =
  match msg with
  | ChangePath x ->
      { model with Path = x}, Cmd.none, NoOp

  | SubmitForm ->
      model, Cmd.none, ExternalMsg.Submit model.Path

  | Msg.Cancel ->
      model, Cmd.none, ExternalMsg.Cancel

  | Error ->
        model, Cmd.none, NoOp


let view model (dispatch: Msg -> unit) =
  Modal.modal [ Modal.IsActive true ]
      [ Modal.background [ Props [ OnClick (fun _ -> dispatch Msg.Cancel) ] ] [ ]
        Modal.content [ ]
          [ Box.box' [ ] [
              Heading.p [ Heading.Is5 ] [ str "Select scenario file" ]
              form [] [
                formItem 
                    "File path" 
                    model.Path
                    ChangePath 
                    model.CheckFields
                    (fun _ -> true)
                    "Please enter a valid path"
                    None
                    dispatch


                Field.div [ ]
                    [ File.file [ 
                          File.HasName
                          File.Option.Props [
                            OnChange (fun ev -> !!ev.target?value |> ChangePath |> dispatch) ]]
                        [ File.label [ ]
                            [ File.input [  ]
                              File.cta [ ]
                                [ File.icon [ ]
                                    [ Icon.faIcon [ ] [ Fa.icon Fa.I.Upload ] ]
                                  File.label [ ]
                                    [ str "Choose a file..." ] ]
                              File.name [ ]
                                [ str model.Path ] ] ] ]
            
                hr []
                Button.button 
                    [ Button.OnClick (fun _ -> dispatch SubmitForm)
                      Button.Color Color.IsPrimary ]
                    [str "Submit"]
                Button.button 
                    [ Button.OnClick (fun _ -> dispatch Msg.Cancel)
                      Button.Color Color.IsGrey ]
                    [str "Cancel"]         
              ]
          ]
          ]
        Modal.close [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick (fun _ -> dispatch Msg.Cancel) ] [ ] ]   

