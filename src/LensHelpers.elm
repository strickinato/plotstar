module LensHelpers exposing (..)

import Monocle.Iso exposing (Iso)


floatToStringLens : Float -> Iso Float String
floatToStringLens default =
    { get = String.fromFloat
    , reverseGet = \x -> Maybe.withDefault default (String.toFloat x)
    }


intToStringLens : Int -> Iso Int String
intToStringLens default =
    { get = String.fromInt
    , reverseGet = \x -> Maybe.withDefault default (String.toInt x)
    }
