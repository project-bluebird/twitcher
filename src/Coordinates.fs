module Twitcher.CoordinateSystem

open Twitcher.Domain

// Functions for translating latitude and longitude to x-y coordinates
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
let llaToEcef (latitude: float) (longitude: float) (altitude: float) =

    // WGS84 ellipsoid constants:
    let a = 6378137.0
    let e = 8.1819190842622e-2

    // intermediate calculation
    // (prime vertical radius of curvature)
    let N = a / sqrt(1.0 - e**2.0 * sin(latitude)**2.0)

    // results:
    let x = (N+altitude) * cos(latitude) * cos(longitude)
    let y = (N+altitude) * cos(latitude) * sin(longitude)
    let z = ((1.0-e**2.) * N + altitude) * sin(latitude)

    { X = x; Y = y; Z = z }

let feetToMeters f = float f / 3.2808

let positionToCoordinates (position: PositionInfo) =
  llaToEcef position.lat position.lon (feetToMeters position.alt)