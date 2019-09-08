module Twitcher.CoordinateSystem

open Twitcher.Domain

// https://wiki.openstreetmap.org/wiki/Mercator#C.23_implementation

module Mercator =
  let rMajor = 6378137.0
  let rMinor = 6356752.3142
  let ratio = rMinor / rMajor
  let eccent = sqrt (1.0 - (ratio**2.0))
  let com = 0.5 * eccent

  let deg2rad = System.Math.PI / 180.0

  let degToRad deg = deg * deg2rad
  let radToDeg rad = rad / deg2rad

  let lonToX longitude = rMajor * (longitude * deg2rad)
  
  let latToY latitude =
    let lat' = min 89.5 (max latitude -89.5)
    let phi = lat' * deg2rad
    let sinphi = sin phi
    let con = eccent * sinphi
    let con' = ((1.0 - con) / (1.0 + con))**com
    let ts = tan(0.5 * ((System.Math.PI * 0.5) - phi)) / con'
    - rMajor * log ts  

  let lonLatToXY longitude latitude =
    (lonToX longitude, latToY latitude)

  let xToLon x =
    (radToDeg x)/rMajor

  let yToLat y =
    let ts = System.Math.Exp(-y / rMajor)
    let initialPhi = System.Math.PI/2. - 2. * System.Math.Atan(ts)

    let rec compute phi dphi iter =
      if iter >= 15 || abs(dphi) <= 0.000000001 then
        phi
      else 
        let con = eccent * System.Math.Sin(phi)
        let dphi' = System.Math.PI/2.0 - 2. * System.Math.Atan(ts * System.Math.Pow((1.0 - con) / (1.0 + con), com)) - phi
        compute (phi + dphi') dphi' (iter + 1)

    let phi = compute initialPhi 1.0 0
    radToDeg(phi)    

  let xyToLonLat x y =
    xToLon x, yToLat y


// let (minAltitude: Altitude), (maxAltitude: Altitude) = 0.0<ft>, 45000.0<ft>   


// // Entire Earth area
// let rangeXMin, rangeYMin = Mercator.lonLatToXY -180.0 -89.5
// let rangeXMax, rangeYMax = Mercator.lonLatToXY 180.0 89.5

// // Hard-coded college airspace area
// // TODO Load from some config file
// let sectorXMin, sectorYMin = Mercator.lonLatToXY -2.5 50.3 //-3.974833333 49.0675 
// let sectorXMax, sectorYMax = Mercator.lonLatToXY 1.4 52.0 //5.090666667 53.80883333

// // Around Equator for testing purposes
// let testXMin, testYMin = Mercator.lonLatToXY -2.0 -1.0
// let testXMax, testYMax = Mercator.lonLatToXY 2.0 1.0

/// Linear rescaling
let scale rMin rMax tMin tMax value =
  (value - rMin)/(rMax - rMin) * (tMax - tMin) + tMin

/// Mercator coordinates to visualization coordinates, whole Earth
// let rescaleEarth (longitude, latitude) (xWidth, yWidth) =
//   scale rangeXMin rangeXMax 0. xWidth longitude,
//   scale rangeYMin rangeYMax 0. yWidth latitude

/// Mercator coordinates to visualization coordinates
/// Rescales 
let rescaleSectorToView (sectorDisplay: Model.SectorDisplay) 
      (longitude: float<longitude>, latitude: float<latitude>, altitude: Altitude) 
      (sectorView: Model.SectorView) =
  let xMercator,yMercator = Mercator.lonLatToXY (float longitude) (float latitude)
  let sectorXMin, sectorYMin = sectorView.SectorDisplayArea.BottomLeft
  let sectorXMax, sectorYMax = sectorView.SectorDisplayArea.TopRight
  let xWidth, yWidth = sectorView.VisualisationViewSize

  match sectorDisplay with
  | Model.SectorDisplay.TopDown ->
      // map longitude and latitude to x and y
      scale sectorXMin sectorXMax 0.0 xWidth xMercator,
      yWidth - (scale sectorYMin sectorYMax 0.0 yWidth yMercator)
  
  | Model.SectorDisplay.LateralNorthSouth ->
      // map longitude to x and altitude to y
      scale sectorXMin sectorXMax 0.0 xWidth xMercator,
      yWidth - (scale (float sectorView.SectorDisplayArea.BottomAltitude) (float sectorView.SectorDisplayArea.TopAltitude) 0.0 yWidth (float altitude))

  | Model.SectorDisplay.LateralEastWest ->
      // map latitude to x and altitude to y
      scale sectorYMin sectorYMax 0.0 xWidth yMercator,
      yWidth - (scale (float sectorView.SectorDisplayArea.BottomAltitude) (float sectorView.SectorDisplayArea.TopAltitude) 0.0 yWidth (float altitude))



// let rescaleViewToSector (x, y) (xWidth, yWidth) : float<longitude> * float<latitude> =
//   // rescale from display coordinates to Mercator values
//   let xMercator = scale 0. xWidth sectorXMin sectorXMax x 
//   let yMercator = scale 0. yWidth sectorYMin sectorYMax y

//   // Recompute from Mercator location to actual longitude and latitude
//   let lon, lat = Mercator.xyToLonLat xMercator yMercator
//   lon * 1.<longitude>, lat * 1.<latitude>


/// Mercator coordinates to visualization coordinates, area around Equator
// let rescaleTest (longitude, latitude) (xWidth, yWidth) =
//   scale testXMin testXMax 0. xWidth longitude,
//   scale testYMin testYMax 0. yWidth latitude

let isInViewSector coordinates sectorView =
  let x,y = rescaleSectorToView Model.SectorDisplay.TopDown coordinates sectorView
  let xWidth, yWidth = sectorView.VisualisationViewSize
  x >= 0. && x <= xWidth && y >= 0. && y <= yWidth


let clockwiseAngle (point1: Position) (point2: Position) =
    let center (v1: Coordinates) (v2: Coordinates) = 
      // make point1 cente of the coordinate system
       {Longitude = v2.Longitude - v1.Longitude
        Latitude = v2.Latitude - v1.Latitude }

    let norm (v: Coordinates) = 
      { Longitude = v.Longitude / sqrt(float v.Latitude**2.0 + float v.Longitude**2.0) 
        Latitude = v.Latitude / sqrt(float v.Latitude**2.0 + float v.Longitude**2.0)}    

    let x = { Longitude = 0.0<longitude>; Latitude = 1.0<latitude> }
    let y = center point1.Coordinates point2.Coordinates |> norm
    
    let dot = float x.Latitude * float y.Latitude + float x.Longitude* float y.Longitude
    let det = float x.Longitude * float y.Latitude - float x.Latitude*float y.Longitude
    let angle = 
      System.Math.Atan2 (det, dot) * 180.0/System.Math.PI // clockwise angle
      |> fun a -> if a < 0. then -a else 360.-a
      |> fun a -> if System.Double.IsNaN(a) then 0. else a 
    angle


// For visualisation purposes, these two points are in the centre of the training sector and 5 nautical miles from each other
let calibrationPoint1 = (-0.5<longitude>, 51.<latitude>, 0.0<ft>)
let calibrationPoint2 = (-0.5<longitude>, 51.08323664811<latitude>, 0.0<ft>)

//=======================================================    

let deg2rad d = d * System.Math.PI/180.


// Functions for translating latitude, longitude and altitude to x-y-z coordinates
// Ported from Matlab https://uk.mathworks.com/matlabcentral/fileexchange/7942-covert-lat-lon-alt-to-ecef-cartesian
// # y = ECEF Y-coordinate (m)
// # z = ECEF Z-coordinate (m)
// # lat = geodetic latitude (radians)
// # lon = longitude (radians)
// # alt = height above WGS84 ellipsoid (m)
// # 
// # Notes: This function assumes the WGS84 model.
// #        Latitude is customary geodetic (not geocentric).
// # 
// # Source: "Department of Defense World Geodetic System 1984"
// #         Page 4-4
// #         National Imagery and Mapping Agency
// #         Last updated June, 2004
// #         NIMA TR8350.2
// # 
// # Michael Kleder, July 2005

/// Convert a lat [degrees], lon [degrees], altitude [m] N-by-3 array of geodetic coordinates (latitude, longitude and altitude) 
/// lla, to an N-by-3 array of ECEF coordinates, p. lla is in [degrees degrees meters]. p is in meters. 
/// The default ellipsoid planet is WGS84. Latitude and longitude values can be any value. 
/// Notes: latitude values of +90 and -90 may return unexpected values because of singularity at the poles.
let llaToEcef (latitude: float<latitude>) (longitude: float<longitude>) (altitude: float<m>) =

    // translate latitude and longitude to radians
    let rlatitude = float latitude |> deg2rad
    let rlongitude = float longitude |> deg2rad

    // WGS84 ellipsoid constants:
    let a = 6378137.0
    let e = 8.1819190842622e-2

    // intermediate calculation
    // (prime vertical radius of curvature)
    let N = a / sqrt(1.0 - e**2.0 * sin(rlatitude)**2.0)

    // results:
    let x = (N+float altitude) * cos(rlatitude) * cos(rlongitude) * 1.<m>
    let y = (N+float altitude) * cos(rlatitude) * sin(rlongitude) * 1.<m>
    let z = ((1.0-e**2.) * N + float altitude) * sin(rlatitude) * 1.<m>

    (x,y,z)


let positionToCartesian (position: Position) =
  llaToEcef 
    position.Coordinates.Latitude 
    position.Coordinates.Longitude
    (position.Altitude |> Conversions.Altitude.ft2m)


//==================================================
// Great-circle distance between two points

let greatCircleDistance (position1: Position) (position2: Position) =

  let mAltitude (pos: Position) =
    pos.Altitude  |> Conversions.Altitude.ft2m

  let altitude = (mAltitude position1 + mAltitude position2)/2. 

  let radius = 6371000.<m> + altitude // Radius of the earth in meters + mean altitude
  let dLat = deg2rad (float (position1.Coordinates.Latitude - position2.Coordinates.Latitude))
  let dLon = deg2rad (float (position1.Coordinates.Longitude - position2.Coordinates.Longitude))
  let a = 
    sin(dLat/2.) * sin(dLat/2.) +
    cos(deg2rad(float position1.Coordinates.Latitude)) * cos(deg2rad(float position2.Coordinates.Latitude)) * 
    sin(dLon/2.) * sin(dLon/2.)
    
  let c = 2. * System.Math.Atan2(sqrt(a), sqrt(1.-a))
  radius * c // Distance in meters
