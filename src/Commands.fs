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
    text 
    |> Array.filter (fun line -> line.Trim().StartsWith (name + ":"))
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
    Aircraft_type = getYamlAttribute "aircraft_type" text
    Latitude = getYamlAttribute "latitude" text
    Longitude = getYamlAttribute "longitude" text
    Altitude = getYamlAttribute "altitude" text
    Ground_speed = getYamlAttribute "ground_speed" text
    Simulator_time = getYamlAttribute "simulator_time" text
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
      let url = urlAircraftPosition config + "?acid=all"

      try
        let! res = Fetch.fetch url [ RequestProperties.Method HttpMethod.GET ]
        match res.Status with
          | 200 | 400 -> return true
          | _ -> return false
      with e ->
          if e.Message.StartsWith("400") then   
            return true 
          else 
            return false
  }

let pingBluebirdCmd config = 
  Cmd.ofPromise pingBluebird config ConnectionActive ConnectionError   

// =============================================================== 
// Aircraft position  


type JsonPositionInfo = {
    actype: string
    alt: float<m>
    gs: float<m/s>
    lat: float<latitude>
    lon: float<longitude>
    vs: float<m/s>
}

let positionDecoder = Decode.Auto.generateDecoder<obj>()

let parseAircraftInfo id info =
    {
      AircraftID = id
      Time = None 
      Type = Some info.actype
      Position = {
        Altitude = info.alt |> Conversions.Altitude.m2ft
        Coordinates = {
          Latitude = info.lat
          Longitude = info.lon
        }
      }
      GroundSpeed = info.gs |> Conversions.Speed.ms2knot |> Some 
      VerticalSpeed = info.vs |> Conversions.Speed.ms2fm |> Some
      CalibratedAirSpeed = None
      Heading = None
    }

let parseAllPositions (data: Map<string, obj>) =
  data
  |> Map.toArray
  |> Array.filter (fun (key, info) -> key <> "sim_t")
  |> Array.map (fun (acid, info) -> parseAircraftInfo acid (info :?> JsonPositionInfo))

let getAllPositions config =
  promise {
      let url = 
        urlAircraftPosition config + "?acid=all"
      let! res = Fetch.fetch url [ RequestProperties.Method HttpMethod.GET ]

      match res.Status with
      | 400 -> 
        Browser.console.log("No aircraft in simulation")
        return [||]
      | 200 -> 
        let! txt = res.text()

        let decodeTime = Decode.field "sim_t" (Decode.int)
        let resultTime = Decode.fromString decodeTime txt   // TODO 

        let resultPosition = Decode.fromString (Decode.dict positionDecoder) txt
        let result = Decode.fromString (Decode.dict positionDecoder) txt
        
        match result with
        | Ok values ->
            return parseAllPositions values
        | Error err -> 
            Browser.console.log("Error getting aircraft positions: " + err)
            return [||]
      | _ -> 
        Browser.console.log("Cannot get aircraft positions, return code " + string res.Status)
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

      let! res = Fetch.fetch url [RequestProperties.Method HttpMethod.POST]
      let! txt = res.text()
      match Decode.fromString (Decode.dict positionDecoder) txt with
      | Ok value -> return Some(parseAllPositions value |> Array.exactlyOne)
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
      let! response =  Fetch.fetch url [RequestProperties.Method HttpMethod.POST]
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
      let! response =  Fetch.fetch url [RequestProperties.Method HttpMethod.POST]
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
      let! response =  Fetch.fetch url [RequestProperties.Method HttpMethod.POST]
      match response.Status with
      | 200 -> return true
      | _ -> return false 
  }

let resumeSimulationCmd config =
  Cmd.ofPromise resumeSimulation config ResumedSimulation ConnectionError    

//=============================================================== 
// Change simulation speed (simulation rate multiplier)

let changeSimulationSpeed (config, rate) =
  promise {
      let url = [ urlBase config; config.Endpoint_set_simulation_rate_multiplier ] |> String.concat "/"
      let body = 
        Encode.object [ yield! ["multiplier", Encode.float rate] ]
        |> Encode.toString 0

      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return Some rate
      | 400 -> 
          Browser.console.log("Rate multiplier was invalid")
          return None
      | 500 -> 
          Browser.console.log("Could not change the rate multiplier: " + response.StatusText)
          return None
      | _ -> return None
  }

let changeSimulationRateMultiplierCmd config rate = 
  Cmd.ofPromise changeSimulationSpeed (config, rate) ChangedSimulationRateMultiplier ConnectionError

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
        "alt", Encode.float (float a.Position.Altitude)
        "lat", Encode.float (float a.Position.Coordinates.Latitude)
        "lon", Encode.float (float a.Position.Coordinates.Longitude)
        "hdg", Encode.float a.Heading.Value
        "spd", match a.CalibratedAirSpeed with
                | Some(cas) -> Encode.float (float cas)
                | None -> failwith "Cannot create aircraft"
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
        yield! ["alt", Encode.float (float altitude)] 
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

let changeAltitudeCmd config aircraftID (requestedAltitude: Altitude) verticalSpeed =
  Cmd.ofPromise changeAltitude (config, aircraftID, requestedAltitude, verticalSpeed)
    ChangedAltitude ConnectionError

// =============================================================== 
// Change speed

let urlChangeSpeed (config: Configuration) =
  [ urlBase config
    config.Endpoint_change_speed ]
  |> String.concat "/"

let encodeChangeSpeed aircraftID (speed: Speed) =
  let aircraft = 
    Encode.object 
      [ "acid", Encode.string aircraftID
        "spd", Encode.float (float speed)
      ]
  Encode.toString 0 aircraft  

let changeSpeed (config, aircraftID, cas) =
  promise {
      let url = urlChangeSpeed config
      let body = encodeChangeSpeed aircraftID cas

      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return "Command accepted, speed changed"
      | 400 -> return "Aircraft ID was invalid"
      | 500 -> return "Aircraft not found " + response.StatusText
      | _ -> return response.StatusText    
  }

let changeSpeedCmd config aircraftID calibratedAirSpeed =
  Cmd.ofPromise changeSpeed (config, aircraftID, calibratedAirSpeed)
    ChangedSpeed ConnectionError    

// =============================================================== 
// Change heading

let urlChangeHeading (config: Configuration) =
  [ urlBase config
    config.Endpoint_change_heading ]
  |> String.concat "/"

let encodeChangeHeading (aircraftID: AircraftID) (heading: Heading) =
  let aircraft = 
    Encode.object 
      [ "acid", Encode.string aircraftID
        "hdg", Encode.float (float heading)
      ]
  Encode.toString 0 aircraft  

let changeHeading (config, aircraftID, heading) =
  promise {
      let url = urlChangeHeading config
      let body = encodeChangeHeading aircraftID heading

      let props =
          [ RequestProperties.Method HttpMethod.POST
            Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
            RequestProperties.Body !^body
            ]
      
      let! response =  Fetch.fetch url props
      match response.Status with
      | 200 -> return "Command accepted, heading changed"
      | 400 -> return "Aircraft ID was invalid"
      | 500 -> return "Aircraft not found " + response.StatusText
      | _ -> return response.StatusText    
  }

let changeHeadingCmd config aircraftID heading =
  Browser.console.log("here")
  Cmd.ofPromise changeHeading (config, aircraftID, heading)
    ChangedHeading ConnectionError        

//=================================================================


let sectorDecoder = Decode.Auto.generateDecoder<Coordinates list>()


let getSectorOutline() =
  promise {
    let url = "assets/nats-sector.json"
    try 
      let! res = Fetch.fetch url []
      let! txt = res.text()
      match Decode.fromString sectorDecoder txt with
      | Ok value -> return Some(value)
      | Error err -> 
          Browser.console.log(err)
          return None
    with ex ->
      Browser.console.log(ex)

      // get a default sector
      let urlDefault = "assets/default-sector.json"
      let! resDefault = Fetch.fetch urlDefault []
      let! txt = resDefault.text()
      match Decode.fromString sectorDecoder txt with
      | Ok value -> return Some(value)
      | Error err -> 
          Browser.console.log(err)
          return None
  }


/// Fetch sector outline
let getSectorOutlineCmd() =
  Cmd.ofPromise getSectorOutline () SectorOutline ErrorMessage    

