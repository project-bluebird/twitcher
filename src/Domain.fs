module Twitcher.Domain

[<Measure>] type knot
[<Measure>] type Mach

[<Measure>] type km
[<Measure>] type m
[<Measure>] type ft
[<Measure>] type FL
[<Measure>] type nm // nautical miles

[<Measure>] type h
[<Measure>] type minute
[<Measure>] type s

[<Measure>] type latitude
[<Measure>] type longitude

type AltitudeUnit =
  | Feet
  | Meters
  | FlightLevels

type SpeedUnit =
  | Kmh
  | Knots
  | Mach
  | FeetPerMinute
  | MetersPerSecond  

type AircraftID = string

type Heading = float

type Altitude = float<ft>

type VerticalSpeed = float<ft/minute>

type Speed = float<knot> 

type Coordinates = {
  Latitude : float<latitude>
  Longitude : float<longitude>
}  

type Position = {
  Coordinates : Coordinates
  Altitude : Altitude
}

type AircraftInfo = {
    AircraftID : AircraftID
    Type : string
    Time: System.DateTime option
    Heading : Heading
    Position : Position
    ClearedFlightLevel : Altitude
    RequestedFlightLevel : Altitude option
    GroundSpeed : Speed option
    CalibratedAirSpeed : Speed option
    VerticalSpeed : float<ft/minute> option
}

type FixInfo = {
  Name : string
  Position: Position
}

type Configuration = {
    Host: string
    Port: string
    Api_path: string
    Api_version: string 
    Simulator: string
    Bluesky_simulator: string
    College_simulator: string
    Status_code_aircraft_id_not_found: string
    Endpoint_reset_simulation: string
    Endpoint_pause_simulation: string
    Endpoint_resume_simulation: string
    Endpoint_set_simulation_rate_multiplier: string
    Endpoint_simulation_step : string
    Endpoint_create_scenario: string
    Endpoint_create_aircraft: string
    Endpoint_aircraft_position: string
    Endpoint_change_altitude: string
    Endpoint_change_heading: string
    Endpoint_change_speed: string
    Endpoint_sector: string
    Query_aircraft_id: string
    Aircraft_type: string
    Latitude: string
    Longitude: string
    Altitude: string
    Heading : string
    Ground_speed: string
    Vertical_speed: string
    Simulator_time: string
    Feet_altitude_upper_limit: int
    Flight_level_lower_limit: int
    Endpoint_simulation_info : string
    Endpoint_list_route: string
}
