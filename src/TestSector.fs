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
  name : string
  latitude : float
  longitude : float
  Type : string
}

type PolygonGeometry = {
  Type : string
  coordinates : float list list list
}

type Routes = 
  (string * string[]) list


type PolygonProperties = {
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
  name : string
  Type : string
}

type FeatureGeometry = 
  | LineStringGeometry of LineStringGeometry
  | PolygonGeometry of PolygonGeometry
  | PointGeometry of PointGeometry

type FeatureProperties = 
  | LineStringProperties of LineStringProperties
  | PolygonProperties of PolygonProperties
  | PointProperties of PointProperties

type Feature = {
  Type : string
  geometry : FeatureGeometry
  properties : FeatureProperties
}

type FeatureCollection = {
  features : Feature []
}


let decodePolygonGeometry : Decoder<PolygonGeometry> =
  Decode.object 
    (fun get ->
        {
          Type = get.Required.Field "type" (Decode.string)
          coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float list list list>())
        } 
     )

let decodeGeometry : Decoder<FeatureGeometry> = 
  Decode.field "type" Decode.string
  |> Decode.andThen (
    function
    | "Polygon" ->
      decodePolygonGeometry |> Decode.map PolygonGeometry
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


let decodePolygonProperties : Decoder<PolygonProperties> =
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
      }
  )

let decodeFeature : Decoder<Feature> =
  Decode.object
    (fun get -> 
      { Type = get.Required.Field "type" Decode.string
        geometry = get.Required.Field "geometry" decodeGeometry //(Decode.oneOf [Decode.Auto.generateDecoder<PointGeometry>() |> Decode.map PointGeometry; decodeGeometryCollectionGeometry |> Decode.map GeometryCollectionGeometry; Decode.Auto.generateDecoder<LineStringGeometry>() |> Decode.map LineStringGeometry])
        properties = get.Required.Field "properties" (Decode.oneOf [decodePointProperties |> Decode.map PointProperties; decodePolygonProperties |> Decode.map PolygonProperties; decodeLineStringProperties |> Decode.map LineStringProperties]) })


let decodeFeatureCollection : Decoder<FeatureCollection> =
  Decode.object
    (fun get -> {
      features = get.Required.Field "features" (Decode.array decodeFeature)
    })

let getFixes (fc: FeatureCollection) =
  fc.features
  |> Array.choose (fun f ->
      match f.geometry with
      | PointGeometry pg ->
        match f.properties with
        | PointProperties pp ->
          if pp.Type = "FIX" then
            {
              Name = pp.name 
              Position = {
                Coordinates = {
                  Latitude = pp.latitude * 1.<latitude>
                  Longitude = pp.longitude * 1.<longitude>
                }
                Altitude = 0.<ft>
              }
            }
            |> Some
          else 
            None
        | _ -> None
      | _ -> None)

let getOutline (fc: FeatureCollection) =
  fc.features
  |> Array.choose (fun f ->
      match f.geometry with 
      | PolygonGeometry pg ->
          match f.properties with 
          | PolygonProperties pp ->
              if pp.Type = "SECTOR" then
                let coords = 
                  pg.coordinates
                  |> List.concat 
                  |> List.map (fun l -> { Longitude = l.[0] * 1.<longitude>; Latitude =  l.[1] * 1.<latitude> })   
                  |> Array.ofList          
                {
                  Coordinates = coords
                  TopAltitude = pp.upper_limit.[0] * 1<FL>
                  BottomAltitude = pp.lower_limit.[0] * 1<FL>
                  Waypoints = getFixes fc
                }
                |> Some
              else 
                None
          | _ -> None
      | _ -> None)
  |> Array.exactlyOne

