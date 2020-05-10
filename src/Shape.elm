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


toSquare : Shape -> Shape
toSquare shape =
    case shape of
        Square _ ->
            shape

        Circle circleData ->
            Square
                { height = 2 * circleData.radius
                , width = 2 * circleData.radius
                }


toCircle : Shape -> Shape
toCircle shape =
    case shape of
        Square squareData ->
            Circle { radius = (squareData.height + squareData.width) / 4 }

        Circle _ ->
            shape


label : Shape -> String
label shape =
    case shape of
        Square _ ->
            "Square"

        Circle _ ->
            "Circle"


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
