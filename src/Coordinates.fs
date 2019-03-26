module Twitcher.CoordinateSystem

open Twitcher.Domain

// https://wiki.openstreetmap.org/wiki/Mercator#C.23_implementation

/// Translate latitude and longitude to [x,y] pixel coordinates on a map with Mercator projection
let lonlatToMercator longitude latitude = 
  let rMajor = 6378137.0
  let rMinor = 6356752.3142
  let ratio = rMinor / rMajor
  let eccent = sqrt (1.0 - (ratio**2.0))
  let com = 0.5 * eccent

  let deg2rad = System.Math.PI / 180.0

  let degToRad deg = deg * deg2rad

  let lonToX lon = rMajor * (lon * deg2rad)

  let latToY lat =
    let lat' = min 89.5 (max lat -89.5)
    let phi = lat' * deg2rad
    let sinphi = sin phi
    let con = eccent * sinphi
    let con' = ((1.0 - con) / (1.0 + con))**com
    let ts = tan(0.5 * ((System.Math.PI * 0.5) - phi)) / con'
    - rMajor * log ts

  (lonToX longitude, latToY latitude)


// Entire Earth area
let rangeXMin, rangeYMin = lonlatToMercator -180.0 -89.5
let rangeXMax, rangeYMax = lonlatToMercator 180.0 89.5

// College airspace area
let sectorXMin, sectorYMin = lonlatToMercator -3.974833333 49.0675 
let sectorXMax, sectorYMax = lonlatToMercator 5.090666667 53.80883333

// Around Equator for testing purposes
let testXMin, testYMin = lonlatToMercator -2.0 -1.0
let testXMax, testYMax = lonlatToMercator 2.0 1.0

/// Linear rescaling
let scale rMin rMax tMin tMax value =
  (value - rMin)/(rMax - rMin) * (tMax - tMin) + tMin

/// Mercator coordinates to visualization coordinates, whole Earth
let rescaleEarth (longitude, latitude) (xWidth, yWidth) =
  scale rangeXMin rangeXMax 0. xWidth longitude,
  scale rangeYMin rangeYMax 0. yWidth latitude

/// Mercator coordinates to visualization coordinates, college airspace
let rescaleCollege (longitude, latitude) (xWidth, yWidth) =
  let x,y = lonlatToMercator longitude latitude
  scale sectorXMin sectorXMax 0.0 xWidth x,
  sectorYMax - (scale sectorYMin sectorYMax 0.0 yWidth y)

/// Mercator coordinates to visualization coordinates, area around Equator
let rescaleTest (longitude, latitude) (xWidth, yWidth) =
  scale testXMin testXMax 0. xWidth longitude,
  scale testYMin testYMax 0. yWidth latitude
