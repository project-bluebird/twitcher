module Twitcher.Domain


[<Measure>] type knot
[<Measure>] type Mach
[<Measure>] type km
[<Measure>] type m
[<Measure>] type ft
[<Measure>] type FL
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

type FlightAltitude =
  | FlightLevel of int<FL>
  | Altitude of float<ft>

type VerticalSpeed = float<ft/minute>

type Speed = float<knot> 

type Coordinates = {
  Latitude : float<latitude>
  Longitude : float<longitude>
}  

type Position = {
  Coordinates : Coordinates
  Altitude : FlightAltitude
}

type AircraftInfo = {
    AircraftID : AircraftID
    Type : string option 
    Time: System.DateTime option
    Heading : float option
    Position : Position
    GroundSpeed : Speed option
    CalibratedAirSpeed : Speed option
    VerticalSpeed : float<ft/minute> option
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
    Endpoint_load_scenario: string
    Endpoint_create_aircraft: string
    Endpoint_aircraft_position: string
    Endpoint_change_altitude: string
    Endpoint_change_heading: string
    Endpoint_change_speed: string
    Endpoint_change_vertical_speed: string
    Query_aircraft_id: string
    Latitude: string
    Longitude: string
    Altitude: string
    Ground_speed: string
    Vertical_speed: string
    Feet_altitude_upper_limit: int
    Flight_level_lower_limit: int
}
