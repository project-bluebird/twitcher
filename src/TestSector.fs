module Twitcher.TestSector

open System.Collections.Generic
open Thoth.Json
open Twitcher.Domain
open Twitcher.Model

// ========================
// Properties

type ChildNames = {
  Names : string []
}

type SectorProperties = {
  Name : string
  Type : string
  Children : Dictionary<string, ChildNames>
}

type PointProperties = {
  Name : string
  Type : string
}

type LineStringProperties = {
  Name : string
  Type : string
  Children : Dictionary<string, ChildNames>
}

type PolygonProperties = {
  Name : string
  Type : string
  lower_limit : int
  upper_limit : int
  Children : string option  // how to encode {}?
}

type FeatureProperties = 
  | LineStringProperties of LineStringProperties
  | PolygonProperties of PolygonProperties
  | PointProperties of PointProperties
  | SectorProperties of SectorProperties

// ========================
// Geometries

type PointGeometry = {
  Type : string
  coordinates : float []
}

type LineStringGeometry = {
  Type : string
  coordinates : float [] []
}

type PolygonGeometry = {
  Type : string
  coordinates : float [] [] []
}

// type SectorGeometry = { Type : string option } // use this instead of string option?

type FeatureGeometry = 
  | LineStringGeometry of LineStringGeometry
  | PolygonGeometry of PolygonGeometry
  | PointGeometry of PointGeometry
  | SectorGeometry of string option

// ========================
// General structure

type Feature = {
  Type : string
  geometry : FeatureGeometry
  properties : FeatureProperties
}

type FeatureCollection = {
  features : Feature []
}

// ===========================
// Decoders


let decodePolygonGeometry : Decoder<PolygonGeometry> =
  Decode.object 
    (fun get -> {
      Type = get.Required.Field "type" Decode.string
      coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float [][][]>())
    }
    )
