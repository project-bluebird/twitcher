module Twitcher.Model

open Twitcher.Domain
open System.Collections.Generic

type SimulatorState = 
  | Playing
  | Paused
  | Observing

type TwitcherState = 
  | NotConnected
  | Connected   // after loading config and pinging the Bluebird server
  | ConnectionFailed
  | ActiveSimulation of SimulatorState       // when loaded scenario
  | ReplaySimulation        // replay simulation from log file without server communication


type CommandForm = 
  | CreateAircraftForm of Twitcher.AircraftForm.FormModel
  | ChangeAltitudeForm of Twitcher.AltitudeForm.FormModel
  | ChangeSpeedForm of Twitcher.SpeedForm.FormModel
  | ChangeHeadingForm of Twitcher.HeadingForm.FormModel

type ElapsedTime = System.TimeSpan

type SectorDisplay =
  | TopDown
  | LateralNorthSouth
  | LateralEastWest

type DisplayAreaMercator = {
  BottomLeft : float * float
  TopRight : float * float
  BottomAltitude : float<ft>
  TopAltitude : float<ft>
}  

type DisplayView = {
  VisualisationViewSize : float * float // width, height on screen
  DisplayArea : DisplayAreaMercator  // entire displayed area, not the same as the visualised sector
}  

type SectorInfo = {
  Coordinates : Coordinates [] // outline coordinates
  BottomAltitude : int<FL>
  TopAltitude : int<FL>
  Waypoints : FixInfo []
}

type SimulationInfo = {
    Callsigns : string []
    Mode : string
    Sector_name : string
    Scenario_name : string
    Scenario_time : float
    Seed : int option
    Sim_type : string
    Speed : int
    State : string
    Dt : float
    Utc_datetime : string
}

type Route = {
  NextWaypoint : string
  Name : string
  Waypoints : string []
}

type Model = {
  Animate : bool

  SectorInfo : SectorInfo option
  SectorDisplay : SectorDisplay
  DisplayView : DisplayView   
  ShowWaypoints : bool

  Positions : AircraftInfo list    // TODO - this should contain full aircraft information, not just positions
  PositionHistory : int * Dictionary<AircraftID, Position []>
  InConflict : AircraftID[]
  Config : Configuration option
  State: TwitcherState
  FormModel : CommandForm option
  SimulationViewSize : float * float // width, height
  ViewDetails : (AircraftID * Route option) option
  SeparationDistance : float option  // what does the loss of separation distance look like in pixels
  SimulationSpeed : float
  SimulationTime: ElapsedTime

  SimulationInfo : SimulationInfo option

  Score : float
}


type Msg =
  | Init
  | Config of Configuration
  | GetSimulationInfo
  | SimulationInfo of SimulationInfo option

  | ReadJsonErrorr
  | ReadSectorDefinition of string
  | UploadSector of string //Thoth.Json.JsonValue
  | SectorUploaded of string
  | LoadSector
  | SectorOutline of SectorInfo option

  | ReadScenario of string
  | UploadScenario of string
  | LoadedScenario of string
  
  | ConnectionActive of bool
  | ConnectionError of exn
  | GetSimulationViewSize
  | ChangeDisplay of SectorDisplay

  | ViewAircraftDetails of AircraftID
  | CloseAircraftDetails
  | RouteInfo of (AircraftID * Route option)
  
  | GetPosition of AircraftID
  | GetAllPositions
  | FetchedPosition of AircraftInfo option
  | FetchedAllPositions of AircraftInfo[] * ElapsedTime

  | Observe 
  | StopObserving

  | ResetSimulator 
  | ResetedSimulator of bool

  | PauseSimulation
  | PausedSimulation of bool

  | ShowWaypoints of bool

  | ResumeSimulation
  | ResumedSimulation of bool

  | SetSimulationRateMultiplier of float
  | ChangedSimulationRateMultiplier of float option

  | ShowCreateAircraftForm
  | CreateAircraft of AircraftInfo
  | CreatedAircraft of string

  | ShowChangeAltitudeForm of AircraftInfo
  | ChangeAltitude of AircraftID * Altitude * float option
  | ChangedAltitude of string

  | ShowChangeHeadingForm of AircraftInfo
  | ChangeHeading of AircraftID * Heading
  | ChangedHeading of string

  | ShowChangeSpeedForm of AircraftInfo
  | ChangeSpeed of AircraftID * Speed
  | ChangedSpeed of string

  | ChangeVerticalSpeed of AircraftID * VerticalSpeed
  | ChangedVerticalSpeed
  
  // | ShowLoadScenarioForm 

  | MakeAnimationStep of unit
  | ErrorMessage of exn
  | StartAnimation
  | StopAnimation

  | MakeSimulatorStep 

  | CreateAircraftMsg of AircraftForm.Msg
  | ChangeAltitudeMsg of AltitudeForm.Msg
  | ChangeSpeedMsg of SpeedForm.Msg
  | ChangeHeadingMsg of HeadingForm.Msg

