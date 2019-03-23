module Twitcher.Model

open Twitcher.Domain

type CommandForm = 
  | CreateAircraftForm of Twitcher.AircraftForm.FormModel

type Model = {
  Animate : bool
  Positions : Coordinates list
  Config : Configuration option
  State: TwitcherState
  FormModel : CommandForm option
}

type Msg =
  | Init
  | Config of Configuration
  | ConnectionActive of bool
  | ConnectionError of exn
  
  | GetPosition of AircraftID
  | GetAllPositions
  | FetchedPosition of AircraftInfo option
  | FetchedAllPositions of AircraftInfo[]

  | LoadScenario of string
  | LoadedScenario of string

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

  | ChangeAltitude of AircraftID * FlightAltitude * float option
  | ChangedAltitude

  | ChangeHeading of AircraftID * float
  | ChangedHeading

  | ChangeSpeed of AircraftID * CalibratedAirSpeed
  | ChangedSpeed

  | ChangeVerticalSpeed of AircraftID * float
  | ChangedVerticalSpeed
  
  | MakeStep of unit
  | ErrorMessage of exn
  | StartAnimation
  | StopAnimation

  | CreateAircraftMsg of AircraftForm.Msg