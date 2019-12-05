module Twitcher.TestSector

open System.Collections.Generic
open Thoth.Json
open Twitcher.Domain
open Twitcher.Model

// ========================
// Properties

type FixNames = {
  Names : string []
}

type SectorProperties = {
  Name : string
  Type : string
  Children : Map<string, FixNames>
}

type PointProperties = {
  Name : string
  Type : string
}

type LineStringProperties = {
  Name : string
  Type : string
  Children : Map<string, FixNames>
}

type PolygonProperties = {
  Name : string
  Type : string
  LowerLimit : int
  UpperLimit : int
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

type FeatureGeometry = 
  | LineStringGeometry of LineStringGeometry
  | PolygonGeometry of PolygonGeometry
  | PointGeometry of PointGeometry

// ========================
// General structure

type Feature = {
  Type : string
  Geometry : FeatureGeometry option
  Properties : FeatureProperties
}

type FeatureCollection = {
  Features : Feature []
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

let decodePointGeometry : Decoder<PointGeometry> =
  Decode.object 
    (fun get -> {
      Type = get.Required.Field "type" Decode.string
      coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float []>())
    }
    )    

let decodeLineStringGeometry : Decoder<LineStringGeometry> =
  Decode.object 
    (fun get -> {
      Type = get.Required.Field "type" Decode.string
      coordinates = get.Required.Field "coordinates" (Decode.Auto.generateDecoder<float [][]>())
    }
    )

// -----

let decodePolygonProperties : Decoder<PolygonProperties> =
  Decode.object 
    (fun get -> {
      Name = get.Required.Field "name" Decode.string
      Type = get.Required.Field "type" Decode.string
      LowerLimit = get.Required.Field "lower_limit" Decode.int
      UpperLimit = get.Required.Field "upper_limit" Decode.int
      Children = None
    })   


let decodeFixes : Decoder<FixNames> =
  Decode.object 
    (fun get -> {
      Names = get.Required.Field "names" (Decode.Auto.generateDecoder<string []>())
    })

let decodeLineStringProperties : Decoder<LineStringProperties> =
  Decode.object 
    (fun get -> {
      Name = get.Required.Field "name" Decode.string
      Type = get.Required.Field "type" Decode.string
      Children = get.Required.Field "children" (Decode.dict decodeFixes)
    })    

let decodePointProperties : Decoder<PointProperties> =
  Decode.object 
    (fun get -> {
      Name = get.Required.Field "name" Decode.string
      Type = get.Required.Field "type" Decode.string
    })

let decodeSectorProperties : Decoder<SectorProperties> =
  Decode.object 
    (fun get -> {
      Name = get.Required.Field "name" Decode.string
      Type = get.Required.Field "type" Decode.string
      Children = get.Required.Field "children" (Decode.dict decodeFixes)
    })    

let decodeProperties : Decoder<FeatureProperties> = 
  Decode.field "type" Decode.string
  |> Decode.andThen (
    function
    | "SECTOR" ->
      decodeSectorProperties |> Decode.map SectorProperties
    | "SECTOR_VOLUME" ->
      decodePolygonProperties |> Decode.map PolygonProperties
    | "ROUTE" ->
      decodeLineStringProperties |> Decode.map LineStringProperties
    | "FIX" ->
      decodePointProperties |> Decode.map PointProperties
    | x -> Decode.fail ("Unknown properties " + x)
  )    

//----

let decodeFeature : Decoder<Feature> =
  Decode.object
    (fun get -> 
      let properties = get.Required.Field "properties" decodeProperties 
      { Type = get.Required.Field "type" Decode.string
        //geometry = get.Required.Field "geometry" decodeGeometry
        Properties = properties
        Geometry =
          match properties with
          | LineStringProperties _ -> 
              get.Required.Field "geometry" (decodeLineStringGeometry |> Decode.map LineStringGeometry)
              |> Some
          | PolygonProperties _ -> 
              get.Required.Field "geometry" (decodePolygonGeometry |> Decode.map PolygonGeometry)
              |> Some
          | PointProperties _ ->
              get.Required.Field "geometry" (decodePointGeometry |> Decode.map PointGeometry)
              |> Some
          | SectorProperties _ ->
              None
      })


let decodeFeatureCollection : Decoder<FeatureCollection> =
  Decode.object
    (fun get -> {
      Features = get.Required.Field "features" (Decode.array decodeFeature)
    })

// ======================

let getFixes (fc: FeatureCollection) =
  fc.Features
  |> Array.choose (fun f ->
      match f.Geometry with
      | Some (PointGeometry pg) ->
        match f.Properties with
        | PointProperties pp ->
          if pp.Type = "FIX" then
            {
              Name = pp.Name 
              Position = {
                Coordinates = {
                  Latitude = pg.coordinates.[0] * 1.<latitude>
                  Longitude = pg.coordinates.[1] * 1.<longitude>
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
  fc.Features
  |> Array.choose (fun f ->
      printfn "%A" f
      match f.Geometry with 
      | Some(PolygonGeometry gm) ->
          match f.Properties with 
          | PolygonProperties gp ->
              if gm.Type = "Polygon" then
                let coords = 
                  gm.coordinates
                  |> Array.concat 
                  |> Array.map (fun l -> { Longitude = l.[0] * 1.<longitude>; Latitude =  l.[1] * 1.<latitude> })   
                {
                  Coordinates = coords
                  TopAltitude = gp.UpperLimit * 1<FL>
                  BottomAltitude = gp.LowerLimit * 1<FL>
                  Waypoints = getFixes fc
                }
                |> Some
              else 
                None
          | _ -> None
      | _ -> None)
  |> Array.exactlyOne