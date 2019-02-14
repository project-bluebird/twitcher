module Twitcher.Domain

type PositionInfo = {
    _validTo: string
    alt: int
    gs: float
    lat: float
    lon: float
    vs: float
}

type Coordinates = {
  X : float
  Y : float
  Z : float
}