module Twitcher.Commands

open Twitcher.Domain
open Twitcher.Model

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
open System


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

let getConfigCmd () = 
  Cmd.ofPromise getConfig () Config ConnectionError


let urlBase config = 
  ["http://" + config.Host + ":" + config.Port; 
   config.Api_path
   config.Api_version ] |> String.concat "/"

let urlAircraftPosition (config: Configuration) =
  [urlBase config
   config.Endpoint_aircraft_position ]
  |> String.concat "/"   

let pingBluebird config = 
  promise {
      let url = 
        urlAircraftPosition config + "?acid=all"
      let props =
          [ RequestProperties.Method HttpMethod.GET
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]

      try
        let! res = Fetch.fetch url props
        match res.Status with
        | 200 -> return true
        | _ -> return false
      with e ->
        return false
  }

let pingBluebirdCmd config = 
  Cmd.ofPromise pingBluebird config ConnectionActive ConnectionError   

// =============================================================== 
// Aircraft position  



type JsonPositionInfo = {
    _validTo: string
    alt: float
    gs: float
    lat: float
    lon: float
    vs: float
}

let positionDecoder = Decode.Auto.generateDecoder<JsonPositionInfo>()

let parseAircraftInfo id info =
    {
      AircraftID = id
      Time = DateTime.Parse(info._validTo) |> Some
      Type = None
      Altitude = Altitude info.alt
      Speed = Observed { Ground = info.gs; Vertical = info.vs } |> Some
      Latitude = info.lat
      Longitude = info.lon
      Heading = None
    }

let parseAllPositions (data: Map<string, JsonPositionInfo>) =
  data
  |> Map.toArray
  |> Array.map (fun (id, info) -> parseAircraftInfo id info)

let getAllPositions config =
  promise {
      let url = 
        urlAircraftPosition config + "?acid=all"
      let props =
          [ RequestProperties.Method HttpMethod.GET
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]

      let! res = Fetch.fetch url props
      let! txt = res.text()
      let result =
        Decode.fromString (Decode.dict positionDecoder) txt
      match result with
      | Ok values ->
          return parseAllPositions values
      | Error err -> 
          Browser.console.log("Error getting aircraft positions: " + err)
          return [||]
  }

let getAllPositionsCmd config  =
  Cmd.ofPromise getAllPositions config FetchedAllPositions ConnectionError

// =============================================================== 
// Get single aircraft's position


let getAircraftPosition (config, aircraftID) =
  promise {
      let url = 
        urlAircraftPosition config + "?acid=" + aircraftID
      let props =
          [ RequestProperties.Method HttpMethod.GET
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]

      let! res = Fetch.fetch url props
      let! txt = res.text()
      match Decode.fromString positionDecoder txt with
      | Ok value -> return Some(parseAircraftInfo aircraftID value)
      | Error err -> return None
  }

let getAircraftPositionCmd config aircraftID =
  Cmd.ofPromise getAircraftPosition (config, aircraftID) FetchedPosition ConnectionError

// =============================================================== 
// Load scenario

let urlLoadScenario (config: Configuration) =
  [ urlBase config
    config.Endpoint_load_scenario ]
  |> String.concat "/"

let loadScenario (config, path) =
  promise {
      let url = urlLoadScenario config
      let body = Encode.toString 0 (Encode.object [ "filename", Encode.string path ])
      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return "Scenario loaded"
      | 400 -> return "Invalid filename"
      | 500 -> return "Scenario not found"
      | _ -> return response.StatusText
  }

let loadScenarioCmd config path =
  Cmd.ofPromise loadScenario (config, path) LoadedScenario ConnectionError

// =============================================================== 
// Reset simulator

let urlReset config =
  [ urlBase config
    config.Endpoint_reset_simulation ]
  |> String.concat "/"

let resetSimulator config = 
  promise {
      let url = urlReset config
      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return true
      | _ -> return false 
  }

let resetSimulatorCmd config =
  Cmd.ofPromise resetSimulator config ResetedSimulator ConnectionError

// =============================================================== 
// Pause simulation

let urlPause config = [ urlBase config; config.Endpoint_pause_simulation ] |> String.concat "/"

let pauseSimulation config = 
  promise {
      let url = urlPause config
      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return true
      | _ -> return false 
  }

let pauseSimulationCmd config =
  Cmd.ofPromise pauseSimulation config PausedSimulation ConnectionError  


// =============================================================== 
// Resume simulation

let urlResume config = [ urlBase config; config.Endpoint_resume_simulation ] |> String.concat "/"

let resumeSimulation config = 
  promise {
      let url = urlResume config
      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return true
      | _ -> return false 
  }

let resumeSimulationCmd config =
  Cmd.ofPromise resumeSimulation config ResumedSimulation ConnectionError    

// =============================================================== 
// Create aircraft

let urlCreateAircraft (config: Configuration) =
  [ urlBase config
    config.Endpoint_create_aircraft ]
  |> String.concat "/"


let encodeAircraftInfo a =
  let aircraft = 
    Encode.object 
      [ "acid", Encode.string a.AircraftID
        "type", Encode.string a.Type.Value
        "alt", match a.Altitude with 
               | FlightLevel fl -> Encode.string ("FL" + string fl)
               | Altitude alt -> Encode.float alt
        "lat", Encode.float a.Latitude
        "lon", Encode.float a.Longitude
        "hdg", Encode.float a.Heading.Value
        "spd", match a.Speed with
                | Some(CalibratedAirSpeed cas) -> 
                    match cas with 
                    | Knots s -> Encode.float s
                    | Mach s -> Encode.float s
                | None | Some(Observed _)  ->
                    failwith "Cannot create aircraft"
      ]
  Encode.toString 0 aircraft

let createAircraft (config, aircraftData: AircraftInfo) =
  promise {
      let url = urlCreateAircraft config
      let body = encodeAircraftInfo aircraftData
      Browser.console.log(body)

      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return "Aircraft created"
      | 400 -> return "Aircraft already exists"
      | 500 -> return "Aircraft could not be created: " + response.StatusText
      | _ -> return response.StatusText
  }

let createAircraftCmd config aircraftData =
  Cmd.ofPromise createAircraft (config, aircraftData) CreatedAircraft ConnectionError

// =============================================================== 
// Change altitude

let urlChangeAltitude (config: Configuration) =
  [ urlBase config
    config.Endpoint_change_altitude ]
  |> String.concat "/"

let encodeChangeAltitude aircraftID altitude verticalSpeed =
  let aircraft = 
    Encode.object 
      [ yield! ["acid", Encode.string aircraftID]
        yield! ["alt", 
          match altitude with 
                | FlightLevel fl -> Encode.string ("FL" + string fl)
                | Altitude alt -> Encode.float alt] 
        yield! [
           match verticalSpeed with 
           | Some vs -> yield "vspd", Encode.float vs
           | None -> () ]
      ]
  Encode.toString 0 aircraft  

let changeAltitude (config, aircraftID, requestedAltitude, verticalSpeed) =
  promise {
      let url = urlChangeAltitude config
      let body = encodeChangeAltitude aircraftID requestedAltitude verticalSpeed
      Browser.console.log(body)

      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return "Command accepted, altitude changed"
      | 400 -> return "Aircraft ID was invalid"
      | 500 -> return "Aircraft not found " + response.StatusText
      | _ -> return response.StatusText    
  }

let changeAltitudeCmd config aircraftID requestedAltitude verticalSpeed =
  Cmd.ofPromise changeAltitude (config, aircraftID, requestedAltitude, verticalSpeed)
    ChangedAltitude ConnectionError