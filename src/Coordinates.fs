module Twitcher.CoordinateSystem

open Twitcher.Domain

/// Translate latitude and longitude to [x,y] pixel coordinates on a map
let positionToMercator (position: PositionInfo) =
  
  // https://stackoverflow.com/questions/2103924/mercator-longitude-and-latitude-calculations-to-x-and-y-on-a-cropped-map-of-the/10401734#10401734
  let mapWidth = 1500.
  let mapHeight = 1577.

  let mapLonLeft = -10.0
  let mapLonRight = 2.0
  let mapLonDelta = mapLonRight - mapLonLeft

  let mapLatBottom = 52.0
  let mapLatBottomDegree = mapLatBottom * System.Math.PI/180.0

  let x = (position.lon - mapLonLeft) * (mapWidth / mapLonDelta)
  let latitude = position.lat * System.Math.PI / 180.0
  let worldMapWidth = ((mapWidth / mapLonDelta) * 360.0) / (2.0 * System.Math.PI)
  let mapOffsetY = (worldMapWidth/2.0 * log((1. + sin(mapLatBottomDegree)) / (1. - sin(mapLatBottomDegree))))
  let y = mapHeight - ((worldMapWidth / 2. * log((1. + sin(latitude)) / (1.0 - sin(latitude)))) - mapOffsetY)
  
  { X = x; Y = y; Altitude = position.alt }