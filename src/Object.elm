module Object exposing (..)

import LensHelpers exposing (..)
import Monocle.Compose as Compose
import Monocle.Iso exposing (Iso)
import Monocle.Lens exposing (Lens)
import Monocle.Prism exposing (Prism)
import Shape exposing (Shape)
import Transformation exposing (Transformation)


type alias Object =
    { x : Float
    , y : Float
    , anchorX : Float
    , anchorY : Float
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
    , xShift = Transformation.default
    , yShift = Transformation.default
    , loops = 1
    , rotation = Transformation.default
    , shape = shape
    , scale = Transformation.default
    }
