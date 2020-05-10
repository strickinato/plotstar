module Shape exposing (..)

import Monocle.Lens exposing (Lens)


type Shape
    = Square SquareData
    | Circle CircleData


type alias SquareData =
    { height : Float
    , width : Float
    }


type alias CircleData =
    { radius : Float
    }


radiusLens : Float -> Lens Shape Float
radiusLens defaultValue =
    { get =
        \s ->
            case s of
                Circle cd ->
                    cd.radius

                _ ->
                    defaultValue
    , set =
        \f s ->
            case s of
                Circle cd ->
                    Circle { cd | radius = f }

                _ ->
                    s
    }


widthLens : Float -> Lens Shape Float
widthLens defaultValue =
    { get =
        \s ->
            case s of
                Square sd ->
                    sd.width

                _ ->
                    defaultValue
    , set =
        \f s ->
            case s of
                Square sd ->
                    Square { sd | width = f }

                _ ->
                    s
    }


heightLens : Float -> Lens Shape Float
heightLens defaultValue =
    { get =
        \s ->
            case s of
                Square sd ->
                    sd.height

                _ ->
                    defaultValue
    , set =
        \f s ->
            case s of
                Square sd ->
                    Square { sd | height = f }

                _ ->
                    s
    }


defaultSquare : Shape
defaultSquare =
    Square { width = 50, height = 50 }


defaultCircle : Shape
defaultCircle =
    Circle { radius = 25 }
