module Object exposing (..)

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
