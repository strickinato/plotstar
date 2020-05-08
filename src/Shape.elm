module Shape exposing (..)


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


defaultSquare : Shape
defaultSquare =
    Square { width = 50, height = 50 }


defaultCircle : Shape
defaultCircle =
    Circle { radius = 25 }
