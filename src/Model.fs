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
  | LoadScenarioForm of Twitcher.ScenarioForm.FormModel

type ElapsedTime = System.TimeSpan

type SectorDisplay =
  | TopDown
  | LateralNorthSouth
  | LateralEastWest

// Alternatively - store this already in Mercator projection!!!
type SectorDisplayAreaMercator = {
  BottomLeft : float * float
  TopRight : float * float
  BottomAltitude : float<ft>
  TopAltitude : float<ft>
}  

type Model = {
  Animate : bool

  Sector : Sectors option
  SectorDisplay : SectorDisplay
  SectorDisplayArea : SectorDisplayAreaMercator  // This is the entire area that on visual display

  Positions : AircraftInfo list    // TODO - this should contain full aircraft information, not just positions
  PositionHistory : int * Dictionary<AircraftID, Position []>
  InConflict : AircraftID[]
  Config : Configuration option
  State: TwitcherState
  FormModel : CommandForm option
  SimulationViewSize : float * float // width, height
  ViewDetails : AircraftID option
  SeparationDistance : float option  // what does the loss of separation distance look like in pixels
  SimulationSpeed : float
  SimulationTime: ElapsedTime
  
  TeamCount : int
  TeamScores : float []
}


type Msg =
  | Init
  | Config of Configuration
  | LoadSector
  | SectorOutline of Coordinates list option
  | ConnectionActive of bool
  | ConnectionError of exn
  | GetSimulationViewSize

  | ChangeDisplay of SectorDisplay

  | ViewAircraftDetails of AircraftID
  | CloseAircraftDetails
  
  | GetPosition of AircraftID
  | GetAllPositions
  | FetchedPosition of AircraftInfo option
  | FetchedAllPositions of AircraftInfo[] * ElapsedTime

  | LoadScenario of string
  | LoadedScenario of string
  | Observe 
  | StopObserving

  | ResetSimulator 
  | ResetedSimulator of bool

  | PauseSimulation
  | PausedSimulation of bool

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
  
  | ShowLoadScenarioForm 

  | MakeStep of unit
  | ErrorMessage of exn
  | StartAnimation
  | StopAnimation

  | CreateAircraftMsg of AircraftForm.Msg
  | ChangeAltitudeMsg of AltitudeForm.Msg
  | ChangeSpeedMsg of SpeedForm.Msg
  | ChangeHeadingMsg of HeadingForm.Msg
  | LoadScenarioMsg of ScenarioForm.Msg

  | GetTeamCount
  | GetScores
  | AddScore of (int option *string) option
  | InvalidSeparation of exn

  | FetchedSectorInformation of Sectors option