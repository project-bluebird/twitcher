module Twitcher.Commands

open Twitcher.Domain
open Twitcher.Model
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

open Elmish.React

open Fable.Import
open Fable.Core.JsInterop
open Thoth.Json
open System.Net.Http
open System
open Fable
open Fetch


// TODO: Change this to OpenAPI type provider when implemented

let getYamlAttribute name (text: string []) =
  printfn "%s" name
  let result =
    text
    |> Array.filter (fun line -> line.Trim().StartsWith (name + ":"))
    |> Array.exactlyOne
  result.Remove(0,result.IndexOf ":" + 1)
  |> fun s -> s.Replace("\"","").Trim()


let decodeConfig (alltext: string) =
  let text = alltext.Split '\n'
  {
    Host = getYamlAttribute "host" text //"host.docker.internal" //
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
    Endpoint_create_scenario = getYamlAttribute "endpoint_create_scenario" text
    Endpoint_create_aircraft = getYamlAttribute "endpoint_create_aircraft" text
    Endpoint_aircraft_position = getYamlAttribute "endpoint_aircraft_position" text
    Endpoint_change_altitude = getYamlAttribute "endpoint_change_altitude" text
    Endpoint_change_heading = getYamlAttribute "endpoint_change_heading" text
    Endpoint_change_speed = getYamlAttribute "endpoint_change_speed" text
    Query_aircraft_id = getYamlAttribute "query_aircraft_id" text
    Aircraft_type = getYamlAttribute "aircraft_type" text
    Latitude = getYamlAttribute "latitude" text
    Longitude = getYamlAttribute "longitude" text
    Altitude = getYamlAttribute "altitude" text
    Heading = getYamlAttribute "heading" text
    Ground_speed = getYamlAttribute "ground_speed" text
    Simulator_time = getYamlAttribute "simulator_time" text
    Vertical_speed = getYamlAttribute "vertical_speed" text
    Feet_altitude_upper_limit = getYamlAttribute "feet_altitude_upper_limit" text |> int
    Flight_level_lower_limit = getYamlAttribute "flight_level_lower_limit" text |> int
    Endpoint_sector = getYamlAttribute "endpoint_upload_sector" text
}


let configFile = "assets/api-config.yaml"
let getConfig () =
  promise {
    let url = configFile
    let! res = Fetch.fetch url []
    let! txt = res.text()
    return decodeConfig txt
  }

let getConfigCmd () =
  Cmd.OfPromise.either getConfig () Config ConnectionError


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
      let url = urlAircraftPosition config 

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
  Cmd.OfPromise.either pingBluebird config ConnectionActive ConnectionError

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

let parseAllPositions (data: Microsoft.FSharp.Collections.Map<string, obj>) =
  data
  |> Collections.Map.toArray
  |> Array.filter (fun (key, info) -> key <> "sim_t")
  |> Array.map (fun (acid, info) -> parseAircraftInfo acid (info :?> JsonPositionInfo))

let getAllPositions config =
  promise {
      let url =
        urlAircraftPosition config 
      let! res = Fetch.fetch url [ RequestProperties.Method HttpMethod.GET ]

      match res.Status with
      | 400 ->
        Fable.Core.JS.console.log("No aircraft in simulation")
        return [||], TimeSpan.FromSeconds 0.0
      | 200 ->
        let! txt = res.text()

        let decodeTime = Decode.field "sim_t" (Decode.int)
        let resultTime = Decode.fromString decodeTime txt   // TODO

        let resultPosition = Decode.fromString (Decode.dict positionDecoder) txt
        let result = Decode.fromString (Decode.dict positionDecoder) txt

        match result, resultTime with
        | Ok values, Ok elapsed ->
            return parseAllPositions values, TimeSpan.FromSeconds(float elapsed)
        | Error err, _ | _, Error err ->
            Fable.Core.JS.console.log("Error getting aircraft positions: " + err)
            return [||], TimeSpan.FromSeconds(0.0)
      | _ ->
        Fable.Core.JS.console.log("Cannot get aircraft positions, return code " + string res.Status)
        return [||], TimeSpan.FromSeconds(0.0)
  }

let getAllPositionsCmd config  =
  Cmd.OfPromise.either getAllPositions config FetchedAllPositions ConnectionError

// ===============================================================
// Get single aircraft's position


let getAircraftPosition (config, aircraftID) =
  promise {
      let url =
        urlAircraftPosition config + "?" + config.Query_aircraft_id + "=" + aircraftID

      let! res = Fetch.fetch url [RequestProperties.Method HttpMethod.POST]
      let! txt = res.text()
      match Decode.fromString (Decode.dict positionDecoder) txt with
      | Ok value -> return Some(parseAllPositions value |> Array.exactlyOne)
      | Error err -> return None
  }

let getAircraftPositionCmd config aircraftID =
  Cmd.OfPromise.either getAircraftPosition (config, aircraftID) FetchedPosition ConnectionError

// ===============================================================
// Load scenario

let urlLoadScenario (config: Configuration) =
  [ urlBase config
    config.Endpoint_create_scenario ]
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
  Cmd.OfPromise.either loadScenario (config, path) LoadedScenario ConnectionError

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
  Cmd.OfPromise.either resetSimulator config ResetedSimulator ConnectionError

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
  Cmd.OfPromise.either pauseSimulation config PausedSimulation ConnectionError


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
  Cmd.OfPromise.either resumeSimulation config ResumedSimulation ConnectionError

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
          Fable.Core.JS.console.log("Rate multiplier was invalid")
          return None
      | 500 ->
          Fable.Core.JS.console.log("Could not change the rate multiplier: " + response.StatusText)
          return None
      | _ -> return None
  }

let changeSimulationRateMultiplierCmd config rate =
  Cmd.OfPromise.either changeSimulationSpeed (config, rate) ChangedSimulationRateMultiplier ConnectionError

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
      Fable.Core.JS.console.log(body)

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
  Cmd.OfPromise.either createAircraft (config, aircraftData) CreatedAircraft ConnectionError

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
      Fable.Core.JS.console.log(body)

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
  Cmd.OfPromise.either changeAltitude (config, aircraftID, requestedAltitude, verticalSpeed)
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
  Cmd.OfPromise.either changeSpeed (config, aircraftID, calibratedAirSpeed)
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
  Fable.Core.JS.console.log("here")
  Cmd.OfPromise.either changeHeading (config, aircraftID, heading)
    ChangedHeading ConnectionError

//=================================================================


let sectorDecoder = Decode.Auto.generateDecoder<Coordinates list>()


let urlSector (config: Configuration) =
  [ urlBase config
    config.Endpoint_sector ]
  |> String.concat "/"

let encodeSector sectorJson =
  let body =
    Encode.object
      [ "name", Encode.string "sector-test"
        "content", sectorJson
      ]
  Encode.toString 0 body  

let readSectorFromFile(config, filePath) = 
  promise {
    // TODO - fetch sector outline from JSON 
    // 1. locally 
    // 2. properly from a user-defined file
    // https://fable.io/repl/#?code=LYewJgrgNgpgBAUSsAlgZwBYDoDKLgAOsAkgHYEQAuAUNQBQBUD1AsiqfgIZRycFEoAxp0ooQpOJhAB3dgHM4GGXEog4ENPCSpM1AJogIcYRIBm7MHFAAneO1NrOAI0OVEOjL2uCMKSjEFKCFteUks0TkJYNF43DEpKAjQALgB6VJhkdGw5PwwIJywxVOoGAEpaEAIYCQAxZ1gsAGEQWywAKTQyf2sq6iqauHqnRoAlGE5A-uq6hpgsccnKLAAFXqTpwe1szYltzAWJqYHZkfmW21p0uBYAeQARBAAZWkoAT2qb8Ey4AF5qOCAuAAbyGKFgLVI-ihcGSkko1nkcAAvq8PvAWGgFP8gXAAD5wJoYTikOQwABq3Ag8BApnhiNJAKBBJwnAAbjBauCYJDoW5afT5EzAQSENZetZFmB5FzYLRYG52H44HQyn8QWCIeI+eqAES6lEAGkJwDAWFI4hgV1ScAAqit7gBBAAqCHlMDcEAIYBE8DowCxyUxcjV-u+UCD4bVOKBwBEPisWLgskoGGF+LgrI5sp52pqbkEeZhAFoAHzp3GgnO8-PqwtQ2vI41NU3my20XGi8WtKUy7lwMsVoHXe5qNAgYAe3yM3G40BgTLN1sW0hW6jXcnEBAAdRVtjCMFslhTnkWgQq1AVcDZKBg0is4bg0rQBHjnhjgOlbLgAG1CVBODQGJdTjdhi3rShOHYQ8DQAXSHQE-wQ2d2AoNxkNnIEkMwnCcKaACgLgXVUKoXUMNwoFnXRIjzFgMiKIYuBblIMg0JVUwIAkGBvzLOByMYoEr1o+BfjgbisEg6wyUoAB+YS0BkugAAYLwEgSr1sTgF2sdUACFemkTRrCwUdgCwHMpUPZpNP8VUOzUhjNO0rBxCgEAtLgAAeYs4A4ri2TcQcHIc59X0oBM6CzTluRrGFuOWSTpJk2w0GgShVOC3CnKsy1ux07zfM4sSAoHctMoE0K30QPLe1JHN7PK2dsuMpzHTQZ0YAAD0oOhhIqRrATVfjZ3giqUG-P9RuCv80ARB8FygcyYqLNwpsw+D1xtR1+GoNYQDkaxIiwYAAGs9oOyI4CVT1vV9a9b2kag8VLOBzsOsyT0hcdYGdQ7BCtZ7Xt6C6PryM9KBwN5SB8XoLQ0IjMg8Ys+AIMjAbeo7rE46ggA&html=DwCwLgtgNgfAUHUBTAhgE3gAm54ElgqbhgAOAtEgI4CuAlgG4C8A5AMID2AdmEj+QBUAnqSQtMAY268erXgA8wAenDQA3JJAoATgGcCTGmABm5ABwssOYLona6pMJl3aJTAEQB9TwAkA8gDKAt5KUHQARrpK2qgSYAB0pNocaDRxdNzxEHRc8QBWuu4wwEq29o5W2DZ2Dk4ubl6+gcGeoRFRMShx5GgcEInJqemZ2bkFRSVltfAlIKgYCMDhKUKSUCi6uh4opBRSPCg5SNpFcDi4aIyYdGgeSFDZuiDkO6Tuaxtb7vePz68TSkuDBmSmWaCE8EQKkgsDgQA&css=BYFwtgNgNAUARgewCYE8AEBvGadoGYIB2IAtAM4CWAXgKYBcaAjAGwAOAHgNwwC+MMAOjABDCoRIBjIiFGEaAJ0zZcSCmVYRhKBngg0uynAHcKSEMAaMADFYCk3XGmA0KAc1CWb9w2gBWAVzIQCjwUSWkaYgYJSJAFB1xhCDdxCjiwMmjY+J9dfRJVeRoJYKJohAh-MEJuPkExVn8QJUdWYSRVQlcGAQAmAFYisAScAmJyanomNgNHEzMLNAGrDhG0EXlXMRJEEBAEMEsh2pggA

    return Encode.object ["content", Encode.string "{}" ]
  }  

let uploadSectorOutline(config, sectorJson :JsonValue) =
  promise {    
    // let path =
    //   match filePath with
    //   | Some x -> x
    //   | None -> "assets/sector-X-sector-X-140-400.geojson"

    let url = urlSector config
    let body = encodeSector sectorJson

    let props =
        [ RequestProperties.Method HttpMethod.POST
          Fetch.requestHeaders [ HttpRequestHeaders.ContentType "application/json" ]
          RequestProperties.Body !^body
          ]

    let! response =  Fetch.fetch url props
    match response.Status with
    | _ -> return response.StatusText
  }

let readSectorDefinitionCmd config filePath =
  Cmd.OfPromise.either readSectorFromFile (config, filePath) UploadSector ErrorMessage

/// Fetch sector outline
let uploadSectorOutlineCmd config sectorJson =
  Cmd.OfPromise.either uploadSectorOutline (config, sectorJson) SectorUploaded ErrorMessage
  

let loadSectorOutline config =
  promise {
    // TODO
    return None
  }

let loadSectorOutlineCmd config =
  Cmd.OfPromise.either loadSectorOutline config SectorOutline ErrorMessage

