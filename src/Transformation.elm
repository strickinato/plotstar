module Transformation exposing (..)

import Random


type Transformation
    = Linear Float
    | Cyclical CycleData
    | Random RandomData


default : Transformation
default =
    Linear 0


type alias CycleData =
    { amplitude : Float, frequency : Float }


type alias RandomData =
    { min : Float, max : Float, seed : Random.Seed }
