module Twitcher.Domain

type AircraftID = string

type FlightAltitude =
  | FlightLevel of int
  | Altitude of float   // altitude in feet

type AircraftInfo = {
    AircraftID : AircraftID
    Time: System.DateTime
    Altitude: float
    GroundSpeed: float
    Latitude: float
    Longitude: float
    VerticalSpeed: float
}

type Coordinates = {
  X : float
  Y : float
  Altitude : float
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

type Model = {
  Animate : bool
  State : Coordinates list
  Config : Configuration option
}

type Msg =
  | Config of Configuration
  | ConnectionError of exn
  
  | GetPosition of AircraftID
  | GetAllPositions
  | FetchedPosition of AircraftInfo option
  | FetchedAllPositions of AircraftInfo[]

  | LoadScenario of string
  | LoadedScenario of string

  | ResetSimulation
  | ResetedSimulation

  | PauseSimulation
  | PausedSimulation

  | ResumeSimulation
  | ResumedSimulation

  | SetSimulationRateMultiplier of float
  | ChangedSimulationRateMultiplier

  | CreateAircraft of AircraftInfo
  | CreatedAircraft

  | ChangeAltitude of AircraftID * FlightAltitude * float option
  | ChangedAltitude

  | ChangeHeading of AircraftID * float
  | ChangedHeading

  | ChangeVerticalSpeed of AircraftID * float
  | ChangedVerticalSpeed
  
  | MakeStep of unit
  | ErrorMessage of exn
  | StartAnimation
  | StopAnimation