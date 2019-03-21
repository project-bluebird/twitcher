module Twitcher.Commands

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
open System.Net.Http


// TODO: Change this to OpenAPI type provider when implemented

let getYamlAttribute name (text: string []) =
  let result = 
    text |> Array.filter (fun line -> line.Trim().StartsWith name)
    |> Array.exactlyOne
  result.Remove(0,result.IndexOf ":" + 1)
  |> fun s -> s.Replace("\"","").Trim()


let decodeConfig (alltext: string) = 
  let text = alltext.Split '\n'
  {
    Host = getYamlAttribute "host" text
    Port = getYamlAttribute "port" text
    Api_path = getYamlAttribute "api_path" text
    Api_version = getYamlAttribute "api_version" text 
    Simulator = getYamlAttribute "simulator" text
    Bluesky_simulator = getYamlAttribute "bluesky_simulator" text
    College_simulator = getYamlAttribute "college_simulator" text
    Status_code_aircraft_id_not_found = getYamlAttribute "status_code_aircraft_id_not_found" text
    Endpoint_reset_simulation = getYamlAttribute "endpoint_reset_simulation" text
    Endpoint_pause_simulation = getYamlAttribute "endpoint_pause_simulation" text
    Endpoint_resume_simulation = getYamlAttribute "endpoint_resume_simulation" text
    Endpoint_set_simulation_rate_multiplier = getYamlAttribute "endpoint_set_simulation_rate_multiplier" text
    Endpoint_load_scenario = getYamlAttribute "endpoint_load_scenario" text
    Endpoint_create_aircraft = getYamlAttribute "endpoint_create_aircraft" text
    Endpoint_aircraft_position = getYamlAttribute "endpoint_aircraft_position" text
    Endpoint_change_altitude = getYamlAttribute "endpoint_change_altitude" text
    Endpoint_change_heading = getYamlAttribute "endpoint_change_heading" text
    Endpoint_change_speed = getYamlAttribute "endpoint_change_speed" text
    Endpoint_change_vertical_speed = getYamlAttribute "endpoint_change_vertical_speed" text
    Query_aircraft_id = getYamlAttribute "query_aircraft_id" text
    Latitude = getYamlAttribute "latitude" text
    Longitude = getYamlAttribute "longitude" text
    Altitude = getYamlAttribute "altitude" text
    Ground_speed = getYamlAttribute "ground_speed" text
    Vertical_speed = getYamlAttribute "vertical_speed" text
    Feet_altitude_upper_limit = getYamlAttribute "feet_altitude_upper_limit" text |> int
    Flight_level_lower_limit = getYamlAttribute "flight_level_lower_limit" text |> int
}


let configFile = "https://raw.githubusercontent.com/alan-turing-institute/dodo/master/config.yml"
let getConfig () =
  promise {
    let url = configFile
    let! res = Fetch.fetch url [ RequestProperties.Method HttpMethod.GET ]
    let! txt = res.text()
    return decodeConfig txt
  }

let urlBase config = 
  ["http://" + config.Host + ":" + config.Port; 
   config.Api_path
   config.Api_version ] |> String.concat "/"

let urlAircraftPosition (config: Configuration) =
  [urlBase config
   config.Endpoint_aircraft_position ]
  |> String.concat "/"


let getSimulationState config =
  promise {
      let url = urlAircraftPosition config + "?acid=all"
      let props =
          [ RequestProperties.Method HttpMethod.GET
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]

      let! res = Fetch.fetch url props
      let! txt = res.text()
      return Decode.Auto.unsafeFromString<PositionInfo[]> txt
  }

