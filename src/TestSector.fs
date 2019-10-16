module Twitcher.TestSector

open System.Collections.Generic
open Thoth.Json
open Twitcher.Domain
open Twitcher.Model

// definitions for test sector GeoJSON

type PointGeometry = {
  Type : string
  coordinates : float []
}

type PointProperties = {
  altitude_unit : string
  name : string
  latitude : float
  longitude : float
  Type : string
}

type MultiPolygon = {
  Type : string
  coordinates : float list list list list
}

type GeometryCollectionGeometry = {
  Type : string
  geometries : MultiPolygon list
}

type Routes = 
  (string * string[]) list


type GeometryCollectionProperties = {
  name : string
  Type : string
  lower_limit : int []
  upper_limit : int []
  routes : Routes option 
}

type LineStringGeometry = {
  Type : string
  coordinates : float list list
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
  | LineStringGeometry of LineStringGeometry
  | GeometryCollectionGeometry of GeometryCollectionGeometry
  | PointGeometry of PointGeometry

type FeatureProperties = 
  | LineStringProperties of LineStringProperties
  | GeometryCollectionProperties of GeometryCollectionProperties
  | PointProperties of PointProperties

type Feature = {
  Type : string
  geometry : FeatureGeometry
  properties : FeatureProperties
}

type FeatureCollection = {
  Type : string
  features : Feature []
}

let decodeMultiPolygon : Decoder<MultiPolygon> =
  Decode.object 
    (fun get -> {
      Type = get.Required.Field "type" Decode.string
      coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float list list list list>())
    }
    )

let decodeGeometryCollectionGeometry : Decoder<GeometryCollectionGeometry> =
  Decode.object 
    (fun get ->
        {
          Type = get.Required.Field "type" (Decode.string)
          geometries = get.Required.Field "geometries" (Decode.list decodeMultiPolygon) 
        } 
     )

let decodeGeometry : Decoder<FeatureGeometry> = 
  Decode.field "type" Decode.string
  |> Decode.andThen (
    function
    | "GeometryCollection" ->
      decodeGeometryCollectionGeometry |> Decode.map GeometryCollectionGeometry
    | "LineString" ->
      Decode.object (fun get ->
        {
          Type = get.Required.Field "type" Decode.string
          coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float list list>())
        })
        |> Decode.map LineStringGeometry
    | "Point" ->
      Decode.object (fun get ->
        {
          Type = get.Required.Field "type" Decode.string
          PointGeometry.coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float []>())
        })
        |> Decode.map PointGeometry
    | x -> Decode.fail ("Unknown geometry " + x)
  )


let decodeGeometryCollectionProperties : Decoder<GeometryCollectionProperties> =
  Decode.object
    (fun get ->
      {
        Type = get.Required.Field "type" Decode.string
        name = get.Required.Field "name" Decode.string
        lower_limit = get.Required.Field "lower_limit" (Decode.Auto.generateDecoder<int[]>())
        upper_limit = get.Required.Field "upper_limit" (Decode.Auto.generateDecoder<int[]>())
        routes = get.Optional.Field "routes" (Decode.keyValuePairs (Decode.Auto.generateDecoder<string[]>()))
      }
    )

let decodePointProperties : Decoder<PointProperties> =
  Decode.object 
    (fun get ->
      {
        Type = get.Required.Field "type" Decode.string
        altitude_unit = get.Required.Field "altitude_unit" Decode.string
        name = get.Required.Field "name" Decode.string
        latitude = get.Required.Field "latitude" Decode.float
        longitude = get.Required.Field "longitude" Decode.float
      })    
  
let decodeLineStringProperties : Decoder<LineStringProperties> =
  Decode.object (
    fun get ->
      {
        Type = get.Required.Field "type" Decode.string
        name = get.Required.Field "name" Decode.string
        points = get.Required.Field "points" (Decode.Auto.generateDecoder<string []>())
        latitudes = get.Required.Field "latitudes" (Decode.Auto.generateDecoder<float []>())
        longitudes = get.Required.Field "longitudes" (Decode.Auto.generateDecoder<float []>())
        altitudes = get.Required.Field "altitudes" (Decode.Auto.generateDecoder<float []>())
      }
  )

let decodeFeature : Decoder<Feature> =
  Decode.object
    (fun get -> 
      { Type = get.Required.Field "type" Decode.string
        geometry = get.Required.Field "geometry" decodeGeometry //(Decode.oneOf [Decode.Auto.generateDecoder<PointGeometry>() |> Decode.map PointGeometry; decodeGeometryCollectionGeometry |> Decode.map GeometryCollectionGeometry; Decode.Auto.generateDecoder<LineStringGeometry>() |> Decode.map LineStringGeometry])
        properties = get.Required.Field "properties" (Decode.oneOf [decodePointProperties |> Decode.map PointProperties; decodeGeometryCollectionProperties |> Decode.map GeometryCollectionProperties; decodeLineStringProperties |> Decode.map LineStringProperties]) })


let decodeFeatureCollection : Decoder<FeatureCollection> =
  Decode.object
    (fun get -> {
      Type = get.Required.Field "type" Decode.string
      features = get.Required.Field "features" (Decode.array decodeFeature)
    })


let getOutline (fc: FeatureCollection) =
  fc.features
  |> Array.choose (fun f ->
      match f.geometry with 
      | GeometryCollectionGeometry gm ->
          match f.properties with 
          | GeometryCollectionProperties gp ->
              if gp.Type = "SECTOR" then
                let g = gm.geometries.[0]
                let coords = 
                  g.coordinates
                  |> List.concat 
                  |> List.concat 
                  |> List.map (fun l -> { Longitude = l.[0] * 1.<longitude>; Latitude =  l.[1] * 1.<latitude> })   
                  |> Array.ofList          
                {
                  Coordinates = coords
                  TopAltitude = gp.upper_limit.[0] * 1<FL>
                  BottomAltitude = gp.lower_limit.[0] * 1<FL>
                }
                |> Some
              else 
                None
          | _ -> None
      | _ -> None)
  |> Array.exactlyOne

