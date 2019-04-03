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

type Model = {
  Animate : bool
  Sector : (Coordinates list) option
  Positions : AircraftInfo list    // TODO - this should contain full aircraft information, not just positions
  PositionHistory : int * Dictionary<AircraftID, Position []>
  InConflict : AircraftID[]
  Config : Configuration option
  State: TwitcherState
  FormModel : CommandForm option
  SimulationViewSize : float * float // width, height
  ViewDetails : AircraftID option
  SeparationDistance : float option  // what does the loss of separation distance look like in pixels
}

type Msg =
  | Init
  | Config of Configuration
  | LoadSector
  | ConnectionActive of bool
  | ConnectionError of exn
  | GetSimulationViewSize

  | ViewAircraftDetails of AircraftID
  | CloseAircraftDetails
  
  | GetPosition of AircraftID
  | GetAllPositions
  | FetchedPosition of AircraftInfo option
  | FetchedAllPositions of AircraftInfo[]

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
  | ChangedSimulationRateMultiplier

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
  
  | MakeStep of unit
  | ErrorMessage of exn
  | StartAnimation
  | StopAnimation

  | CreateAircraftMsg of AircraftForm.Msg
  | ChangeAltitudeMsg of AltitudeForm.Msg
  | ChangeSpeedMsg of SpeedForm.Msg
  | ChangeHeadingMsg of HeadingForm.Msg