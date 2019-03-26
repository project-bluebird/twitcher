module Twitcher.Model

open Twitcher.Domain

type CommandForm = 
  | CreateAircraftForm of Twitcher.AircraftForm.FormModel
  | ChangeAltitudeForm of Twitcher.AltitudeForm.FormModel


type Model = {
  Animate : bool
  Sector : ((float * float) list) option
  Positions : AircraftInfo list    // TODO - this should contain full aircraft information, not just positions
  Config : Configuration option
  State: TwitcherState
  FormModel : CommandForm option
  SimulationViewSize : float * float // width, height
}

type Msg =
  | Init
  | Config of Configuration
  | ConnectionActive of bool
  | ConnectionError of exn
  | GetSimulationViewSize
  
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

  | ShowChangeAltitudeForm of AircraftInfo
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
  | ChangeAltitudeMsg of AltitudeForm.Msg