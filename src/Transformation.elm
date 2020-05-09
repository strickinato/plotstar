module Transformation exposing (..)

import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
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


type alias CycleData =
    { amplitude : Float, frequency : Float }


type alias RandomData =
    { min : Float, max : Float, seed : Random.Seed }
