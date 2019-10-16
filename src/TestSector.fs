module Twitcher.TestSector

open System.Collections.Generic

// definitions for test sector GeoJSON

type PointGeometry = {
  Type : string
  Coordinates : float []
}

type PointProperties = {
  AltitudeUnit : string
  Name : string
  Latitude : float
  Longitude : float
  Type : string
}

type Point = {
  Type : string
  Geometry : PointGeometry
  Properties : PointProperties
}

type GeometryCollectionGeometry = {
  Type : string
  Coordinates : float [][][][]
}

type GeometryCollectionProperties = {
  Name : string
  Type : string
  Lower_limit : int []
  Upper_limit : int []
  Routes : Dictionary<string, string[]>
}

type GeometryCollection = {
  Type : string
  Geometries : GeometryCollectionGeometry
  Properties : GeometryCollectionProperties
}

type LineStringGeometry = {
  Type : string
  Coordinates : float [][]
}

type LineStringProperties = {
  Points : string []
  Latitudes : float []
  Longitudes : float []
  Altitudes : float []
  Name : string
  Type : string
}

type LineString = {
  Type : string
  Geometry : LineStringGeometry
  Properties : LineStringProperties
}

type Feature = 
  | Point of Point
  | GeometryCollection of GeometryCollection
  | LineString of LineString
