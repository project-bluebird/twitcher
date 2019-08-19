module Twitcher.Form

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Elmish.React
open Fable.Import
open Fable.Core.JsInterop

let formItem label value message checkValid isValid warning other dispatch =
  Field.div [  ]
    [ yield!
        [ Label.label [ ] [ str label ] ]
      yield!
        [Input.text [
          Input.Placeholder value
          Input.Value value
          Input.Props
            [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch ) ] ] ]

      yield!
        (if checkValid && not (isValid value) then
          [Help.help
            [  Help.Color IsDanger  ]
            [ str warning ]]
         else [])
      yield!
        (match other with
         | Some(elem) -> [elem]
         | None -> [] )]


let formItemOptions label (options: string list) optionMessage value message checkValid isValid warning dispatch =
  Field.div [] [
    Label.label [ ] [ str label ]

    Field.div [ Field.HasAddons ]
      [ yield!
         [ Select.select [ ]
            [ select [ DefaultValue (options.Head)
                       OnChange (fun ev -> !!ev.target?value |> optionMessage |> dispatch ) ]
                (options
                 |> List.map (fun value ->
                    option [ Value value ][ str value] ))
              ] ]
        yield!
         [Input.text [
            Input.Placeholder value
            Input.Value value
            Input.Props
              [ OnChange (fun ev -> !!ev.target?value |> message |> dispatch )
                 ] ] ]

        yield!
          (if checkValid && not (isValid value) then
            [Help.help
              [  Help.Color IsDanger  ]
              [ str warning ]]
           else [])
      ]
  ]

let checkFloat x =
  let canParse, result = System.Double.TryParse(x)
  canParse

let checkAircraftID (x: string) =
  (x.Length >= 3)
  && (x |> Seq.forall (fun c -> System.Char.IsLetterOrDigit(c)))

let checkAircraftType (x: string) =
  (x.Length >= 3)
  && (x |> Seq.forall (fun c -> System.Char.IsLetterOrDigit(c)))
