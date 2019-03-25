module Twitcher.Domain

type AircraftID = string

type SpeedUnit = Knots | Mach | Kmh
type AltitudeUnit = FlightLevels | Feet | Meters


type FlightAltitude =
  | FlightLevel of int
  | Altitude of float   // altitude in feet

type ObservedSpeed = {
  Vertical : float
  Ground : float
}

type CalibratedAirSpeed =
  | Knots of float
  | Mach of float

type Speed =
  | Observed of ObservedSpeed
  | CalibratedAirSpeed of CalibratedAirSpeed

type AircraftInfo = {
    AircraftID : AircraftID
    Type : string option 
    Time: System.DateTime option
    Heading : float option
    Altitude: FlightAltitude
    Latitude: float
    Longitude: float
    Speed : Speed option
}

type Coordinates = {
  Latitude : float
  Longitude : float
  Altitude : FlightAltitude
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

type SimulatorState = 
  | Playing
  | Paused

type TwitcherState = 
  | NotConnected
  | Connected   // after loading config and pinging the Bluebird server
  | ConnectionFailed
  | ActiveSimulation of SimulatorState       // when loaded scenario
  | ReplaySimulation        // replay simulation from log file without server communication
