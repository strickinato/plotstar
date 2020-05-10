module Object exposing (..)

import LensHelpers exposing (..)
import Monocle.Compose as Compose
import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Prism exposing (Prism)
import Random
import Shape exposing (Shape)
import Transformation exposing (Transformation)


type alias Object =
    { x : Float
    , y : Float
    , anchorX : Float
    , anchorY : Float
    , baseRotation : Float
    , loops : Int
    , xShift : Transformation
    , yShift : Transformation
    , scale : Transformation
    , rotation : Transformation
    , shape : Shape
    }


loopFloorLens : Lens Object Float
loopFloorLens =
    { get = toFloat << .loops
    , set = \flt obj -> { obj | loops = floor flt }
    }


xLens : Lens Object Float
xLens =
    { get = .x
    , set = \flt obj -> { obj | x = flt }
    }


yLens : Lens Object Float
yLens =
    { get = .y
    , set = \flt obj -> { obj | y = flt }
    }


baseRotation : Lens Object Float
baseRotation =
    { get = .baseRotation
    , set = \flt obj -> { obj | baseRotation = flt }
    }


anchorXLens : Lens Object Float
anchorXLens =
    { get = .anchorX
    , set = \flt obj -> { obj | anchorX = flt }
    }


anchorYLens : Lens Object Float
anchorYLens =
    { get = .anchorY
    , set = \flt obj -> { obj | anchorY = flt }
    }


xShiftLens : Lens Object Transformation
xShiftLens =
    { get = .xShift
    , set = \trans obj -> { obj | xShift = trans }
    }


yShiftLens : Lens Object Transformation
yShiftLens =
    { get = .yShift
    , set = \trans obj -> { obj | yShift = trans }
    }


scaleLens : Lens Object Transformation
scaleLens =
    { get = .scale
    , set = \trans obj -> { obj | scale = trans }
    }


rotationLens : Lens Object Transformation
rotationLens =
    { get = .rotation
    , set = \trans obj -> { obj | rotation = trans }
    }


shapeLens : Lens Object Shape
shapeLens =
    { get = .shape
    , set = \shape obj -> { obj | shape = shape }
    }


initWithShape : Int -> Int -> Shape -> Object
initWithShape canvasWidth canvasHeight shape =
    { x = toFloat canvasWidth / 2
    , y = toFloat canvasHeight / 2
    , anchorX = toFloat canvasWidth / 2
    , anchorY = toFloat canvasHeight / 2
    , baseRotation = 0
    , xShift = Transformation.default
    , yShift = Transformation.default
    , loops = 1
    , rotation = Transformation.default
    , shape = shape
    , scale = Transformation.default
    }


examples : Int -> Int -> List ( String, Object )
examples w h =
    [ ( "Napkin", napkin w h )
    , ( "Scorpion Tail", scorpion w h )
    , ( "Spots", spots w h )
    ]


napkin : Int -> Int -> Object
napkin canvasWidth canvasHeight =
    { x = toFloat canvasWidth / 2
    , y = toFloat canvasHeight / 2
    , anchorX = toFloat canvasWidth / 2
    , anchorY = toFloat canvasHeight / 2
    , baseRotation = 0
    , xShift = Transformation.default
    , yShift = Transformation.default
    , loops = 130
    , rotation =
        Transformation.Cyclical
            { amplitude = 38, frequency = 24 }
    , shape = Shape.Square { width = 10, height = 10 }
    , scale = Transformation.Linear 6
    }


scorpion : Int -> Int -> Object
scorpion canvasWidth canvasHeight =
    { x = 213
    , y = 198
    , anchorX = 927
    , anchorY = 710
    , baseRotation = 0
    , xShift = Transformation.Linear 8
    , yShift = Transformation.Linear 8
    , loops = 122
    , rotation = Transformation.Linear 1
    , shape = Shape.Circle { radius = 25 }
    , scale =
        Transformation.Cyclical
            { amplitude = 93, frequency = 7 }
    }


spots : Int -> Int -> Object
spots canvasWidth canvasHeight =
    { x = toFloat canvasWidth / 2
    , y = toFloat canvasHeight / 2
    , anchorX = toFloat canvasWidth / 2
    , anchorY = toFloat canvasHeight / 2
    , baseRotation = 0
    , xShift =
        Transformation.Random
            { min = 0 - (toFloat canvasWidth / 2)
            , max = toFloat canvasWidth / 2
            , seed = Random.initialSeed 0
            }
    , yShift =
        Transformation.Random
            { min = 0 - (toFloat canvasHeight / 2)
            , max = toFloat canvasHeight / 2
            , seed = Random.initialSeed 0
            }
    , loops = 300
    , rotation = Transformation.Linear 0
    , shape = Shape.Circle { radius = 25 }
    , scale =
        Transformation.Random
            { min = -100
            , max = 100
            , seed = Random.initialSeed 0
            }
    }
