module Twitcher.TestSector

open System.Collections.Generic

// definitions for test sector GeoJSON

type PointGeometry = {
  Type : string
  coordinates : float []
}

type PointProperties = {
  altitudeUnit : string
  name : string
  latitude : float
  longitude : float
  Type : string
}

type MultiPolygon = {
  Type : string
  coordinates : float [][][][]
}

type GeometryCollectionGeometry = {
  Type : string
  geometries : MultiPolygon []
}

type GeometryCollectionProperties = {
  name : string
  Type : string
  lower_limit : int []
  upper_limit : int []
  routes : Dictionary<string, string[]>
}

type LineStringGeometry = {
  Type : string
  coordinates : float [][]
}

type LineStringProperties = {
  points : string []
  latitudes : float []
  longitudes : float []
  altitudes : float []
  name : string
  Type : string
}


type FeatureGeometry = 
  | LineStringGeometry
  | GeometryCollectionGeometry
  | PointGeometry

type FeatureProperties = 
  | LineStringProperties
  | GeometryCollectionProperties
  | PointProperties

type Feature = {
  Type : string
  geometry : FeatureGeometry
  properties : FeatureProperties
}

type FeatureCollection = {
  Type : string
  features : Feature []
}