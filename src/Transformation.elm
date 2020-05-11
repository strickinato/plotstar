module Transformation exposing (..)

import Json.Decode exposing (Decoder)
import Json.Encode
import Monocle.Lens exposing (Lens)
import Random


type Transformation
    = Linear Float
    | Cyclical CycleData
    | Random RandomData


default : Transformation
default =
    Linear 0


linearLens : Float -> Lens Transformation Float
linearLens defaultValue =
    { get =
        \t ->
            case t of
                Linear f ->
                    f

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Linear _ ->
                    Linear f

                _ ->
                    t
    }


amplitudeLens : Float -> Lens Transformation Float
amplitudeLens defaultValue =
    { get =
        \t ->
            case t of
                Cyclical d ->
                    d.amplitude

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Cyclical d ->
                    Cyclical { d | amplitude = f }

                _ ->
                    t
    }


frequencyLens : Float -> Lens Transformation Float
frequencyLens defaultValue =
    { get =
        \t ->
            case t of
                Cyclical d ->
                    d.frequency

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Cyclical d ->
                    Cyclical { d | frequency = f }

                _ ->
                    t
    }


minLens : Float -> Lens Transformation Float
minLens defaultValue =
    { get =
        \t ->
            case t of
                Random d ->
                    d.min

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Random d ->
                    Random { d | min = f }

                _ ->
                    t
    }


maxLens : Float -> Lens Transformation Float
maxLens defaultValue =
    { get =
        \t ->
            case t of
                Random d ->
                    d.max

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Random d ->
                    Random { d | max = f }

                _ ->
                    t
    }


randLens : Float -> Lens Transformation Float
randLens defaultValue =
    { get =
        \t ->
            case t of
                Random d ->
                    d.seed

                _ ->
                    defaultValue
    , set =
        \f t ->
            case t of
                Random d ->
                    Random { d | seed = f }

                _ ->
                    t
    }


type alias CycleData =
    { amplitude : Float, frequency : Float }


type alias RandomData =
    { min : Float, max : Float, seed : Float }


label : Transformation -> String
label transformation =
    case transformation of
        Linear _ ->
            "Linear"

        Cyclical _ ->
            "Cyclical"

        Random _ ->
            "Random"


decoder : Decoder Transformation
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map Linear linearDecoder
        , Json.Decode.map Cyclical cyclicalDecoder
        , Json.Decode.map Random randomDecoder
        ]


cyclicalDecoder : Decoder CycleData
cyclicalDecoder =
    Json.Decode.map2 CycleData
        (Json.Decode.field "amplitude" Json.Decode.float)
        (Json.Decode.field "frequency" Json.Decode.float)


randomDecoder : Decoder RandomData
randomDecoder =
    Json.Decode.map3 RandomData
        (Json.Decode.field "min" Json.Decode.float)
        (Json.Decode.field "max" Json.Decode.float)
        (Json.Decode.field "seed" Json.Decode.float)


linearDecoder : Decoder Float
linearDecoder =
    Json.Decode.float


encode : Transformation -> Json.Encode.Value
encode transformation =
    case Debug.log "Tranny:" transformation of
        Linear float ->
            Json.Encode.float float

        Cyclical cycleData ->
            cyclicalEncoder cycleData

        Random randomData ->
            randomEncoder randomData


cyclicalEncoder : CycleData -> Json.Encode.Value
cyclicalEncoder cd =
    Json.Encode.object
        [ ( "amplitude", Json.Encode.float <| cd.amplitude )
        , ( "frequency", Json.Encode.float <| cd.frequency )
        ]


randomEncoder : RandomData -> Json.Encode.Value
randomEncoder rd =
    Json.Encode.object
        [ ( "min", Json.Encode.float <| rd.min )
        , ( "max", Json.Encode.float <| rd.max )
        , ( "seed", Json.Encode.float <| rd.seed )
        ]
