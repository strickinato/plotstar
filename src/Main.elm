port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Json.Decode
import Maybe.Extra as Maybe
import Random
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, transform, viewBox, width, x, y)


port dragEvents : (Json.Decode.Value -> msg) -> Sub msg


port gotSvg : (String -> msg) -> Sub msg


port getSvg : String -> Cmd msg


type alias Model =
    { objects : Dict Id Object
    , selectedObjectId : Maybe Id
    , currentDraggable : Maybe Id
    , guidesVisible : Bool
    }


type alias Id =
    Int


type Msg
    = Drag DragMsg
    | GetSvg
    | GotSvg String
    | SetLoops String
    | SetX String
    | SetY String
    | SetAnchorX String
    | SetAnchorY String
      -- | SetWidth String
      -- | SetHeight String
    | SetXShift Transformation
    | SetYShift Transformation
    | SetScale Transformation
    | SetRotation Transformation
    | Center
    | Delete
    | SelectObject (Maybe Id)
    | AddSquare
    | AddCircle -- Todo generalize
    | SetGuidesVisible Bool
    | NoOp


type alias Object =
    { x : Float
    , y : Float
    , anchorX : Int
    , anchorY : Int
    , loops : Int
    , xShift : Transformation
    , yShift : Transformation
    , scale : Transformation
    , rotation : Transformation
    , shape : Shape
    }


type Transformation
    = Linear Float
    | Cyclical CycleData
    | Random RandomData


type alias CycleData =
    { amplitude : Float, frequency : Float }


type alias RandomData =
    { min : Float, max : Float, seed : Random.Seed }


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


type alias DragMsg =
    { event : DragEvent
    , cursor : Coords
    , draggables : List ( Id, Coords )
    }


type DragEvent
    = Start
    | Move
    | Stop


type alias Coords =
    { x : Float, y : Float }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , view = \m -> { title = "Plotter Otter", body = [ view m ] }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map Drag <| dragEvents decodeDragEvents
        , gotSvg GotSvg
        ]


init : Model
init =
    { objects =
        Dict.fromList
            [ ( 0, initShape defaultCircle ) ]
    , selectedObjectId = Just 0
    , currentDraggable = Nothing
    , guidesVisible = True
    }


initShape : Shape -> Object
initShape shape =
    { x = canvasWidth / 2
    , y = canvasHeight / 2
    , anchorX = canvasWidth // 2
    , anchorY = canvasHeight // 2
    , xShift = Linear 0
    , yShift = Linear 0
    , loops = 1
    , rotation = Linear 0
    , shape = shape
    , scale = Linear 0
    }


defaultSquare : Shape
defaultSquare =
    Square { width = 50, height = 50 }


defaultCircle : Shape
defaultCircle =
    Circle { radius = 25 }


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , internalRotation : Float
    , anchorX : Float
    , anchorY : Float
    }


type alias Path =
    List { x : Float, y : Float }


type alias Bounds =
    List { height : Int, width : Int }


type BoundedRectangle
    = In Rect
    | Clipped Path
    | Out


type alias BoundingBox =
    { x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }


cut : Rect -> Bounds -> BoundedRectangle
cut rect bounds =
    Out


getBoundingBox : Rect -> BoundingBox
getBoundingBox rect =
    { x1 = 0
    , x2 = 0
    , y1 = 0
    , y2 = 0
    }


svgId =
    "plotter-otter-svg"



-- Update logic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Drag { event, cursor, draggables } ->
            let
                newModel =
                    case event of
                        Start ->
                            { model
                                | currentDraggable =
                                    draggables
                                        |> List.filter
                                            (\( _, draggable ) ->
                                                distance cursor draggable < 20
                                            )
                                        |> closestRect cursor
                            }

                        Move ->
                            model.currentDraggable
                                |> Maybe.map (moveDraggable model cursor)
                                |> Maybe.withDefault model

                        Stop ->
                            { model | currentDraggable = Nothing }
            in
            ( newModel, Cmd.none )

        GetSvg ->
            ( model, getSvg svgId )

        GotSvg output ->
            ( model, download "your-svg" output )

        SetRotation transformation ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | rotation = transformation })
                        model.objects
              }
            , Cmd.none
            )

        SetLoops str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | loops = Maybe.withDefault 0 <| String.toInt str })
                        model.objects
              }
            , Cmd.none
            )

        SetX str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | x = Maybe.withDefault 0 <| String.toFloat str })
                        model.objects
              }
            , Cmd.none
            )

        SetY str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | y = Maybe.withDefault 0 <| String.toFloat str })
                        model.objects
              }
            , Cmd.none
            )

        -- SetWidth str ->
        --     ( { model
        --         | objects =
        --             updateObject
        --                 model.selectedObjectId
        --                 (\x -> { x | width = Maybe.withDefault 0 <| String.toInt str })
        --                 model.objects
        --       }
        --     , Cmd.none
        --     )
        -- SetHeight str ->
        --     ( { model
        --         | objects =
        --             updateObject
        --                 model.selectedObjectId
        --                 (\x -> { x | height = Maybe.withDefault 0 <| String.toInt str })
        --                 model.objects
        --       }
        --     , Cmd.none
        --     )
        SetXShift transformation ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | xShift = transformation })
                        model.objects
              }
            , Cmd.none
            )

        SetYShift transformation ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | yShift = transformation })
                        model.objects
              }
            , Cmd.none
            )

        SetScale transformation ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | scale = transformation })
                        model.objects
              }
            , Cmd.none
            )

        SetAnchorX str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | anchorX = Maybe.withDefault 0 <| String.toInt str })
                        model.objects
              }
            , Cmd.none
            )

        SetAnchorY str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | anchorY = Maybe.withDefault 0 <| String.toInt str })
                        model.objects
              }
            , Cmd.none
            )

        Center ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | x = canvasWidth / 2, y = canvasHeight / 2 })
                        model.objects
              }
            , Cmd.none
            )

        Delete ->
            let
                newObjects =
                    case model.selectedObjectId of
                        Just id ->
                            Dict.remove id model.objects

                        Nothing ->
                            model.objects
            in
            ( { model
                | selectedObjectId = Nothing
                , objects = newObjects
              }
            , Cmd.none
            )

        SelectObject maybeObjectId ->
            ( { model | selectedObjectId = maybeObjectId }
            , Cmd.none
            )

        AddSquare ->
            let
                ( newSelectedId, newObjects ) =
                    insertShape defaultSquare model.objects
            in
            ( { model | objects = newObjects, selectedObjectId = Just newSelectedId }
            , Cmd.none
            )

        AddCircle ->
            let
                ( newSelectedId, newObjects ) =
                    insertShape defaultCircle model.objects
            in
            ( { model | objects = newObjects, selectedObjectId = Just newSelectedId }
            , Cmd.none
            )

        SetGuidesVisible bool ->
            ( { model | guidesVisible = bool }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


download : String -> String -> Cmd msg
download fileName svg =
    Download.string (String.append fileName ".svg") "image/svg+xml" svg


insertShape : Shape -> Dict Int Object -> ( Int, Dict Int Object )
insertShape shape dict =
    let
        newKey =
            nextKey dict
    in
    ( newKey, Dict.insert newKey (initShape shape) dict )


nextKey : Dict Int Object -> Int
nextKey dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


updateObject : Maybe Id -> (Object -> Object) -> Dict Int Object -> Dict Int Object
updateObject maybeId updater objects =
    case maybeId of
        Just selectedId ->
            Dict.update selectedId
                (Maybe.map updater)
                objects

        Nothing ->
            objects


moveDraggable : Model -> Coords -> Id -> Model
moveDraggable model coords id =
    { model
        | objects =
            Dict.update id
                (updateObjectLocation coords)
                model.objects
    }


updateObjectLocation : Coords -> Maybe Object -> Maybe Object
updateObjectLocation coords maybeObject =
    maybeObject
        |> Maybe.map
            (\object ->
                { object | x = coords.x, y = coords.y }
            )



-- View logic


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.id "main"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "padding" "24px"
        ]
        [ Html.div
            [ Html.Attributes.style "border" "2px solid black"
            , Html.Attributes.id svgId
            ]
            [ Svg.svg
                [ Html.Attributes.style "width" (String.fromInt canvasWidth ++ "px")
                , Html.Attributes.style "height" (String.fromInt canvasHeight ++ "px")
                , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
                ]
                [ viewCenterPoint model.guidesVisible
                , viewObjects model
                ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "padding-left" "24px"
            ]
            [ Html.h3 [] [ Html.text "Controls" ]
            , viewSlider model
                "Loops"
                (String.fromInt << .loops)
                SetLoops
                "1"
                "360"
            , viewSlider model
                "Anchor X"
                (String.fromInt << .anchorX)
                SetAnchorX
                "0"
                (String.fromFloat canvasWidth)
            , viewSlider model
                "Anchor Y"
                (String.fromInt << .anchorY)
                SetAnchorY
                "0"
                (String.fromFloat canvasHeight)
            , viewSlider model
                "X"
                (String.fromFloat << .x)
                SetX
                "0"
                (String.fromInt canvasWidth)
            , viewSlider model
                "Y"
                (String.fromFloat << .y)
                SetY
                "0"
                (String.fromInt canvasHeight)
            , Html.button [ Html.Events.onClick Center ] [ Html.text "Center" ]
            , controlContainer "Rotation" <|
                viewTransformation model .rotation SetRotation
            , controlContainer "X-Shift" <|
                viewTransformation model .xShift SetXShift
            , controlContainer "Y-Shift" <|
                viewTransformation model .yShift SetYShift
            , controlContainer "Scale" <|
                viewTransformation model .scale SetScale

            -- , viewSlider model
            --     "Width"
            --     (String.fromInt << .width)
            --     SetWidth
            --     "0"
            --     "400"
            -- , viewSlider model
            --     "Height"
            --     (String.fromInt << .height)
            --     SetHeight
            --     "0"
            --     "400"
            , viewObjectSelector model
            , Html.button [ Html.Events.onClick AddSquare ] [ Html.text "Another Square" ]
            , Html.button [ Html.Events.onClick AddCircle ] [ Html.text "Another Circle" ]
            , toggle SetGuidesVisible model.guidesVisible "Show Guides"
            , case model.selectedObjectId of
                Just _ ->
                    Html.button [ Html.Events.onClick Delete ] [ Html.text "delete selected" ]

                Nothing ->
                    Html.text ""
            , Html.button [ Html.Events.onClick GetSvg ] [ Html.text "download" ]
            ]
        ]


controlContainer : String -> Html Msg -> Html Msg
controlContainer name control =
    Html.div
        [ Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "margin-bottom" "8px"
        ]
        [ Html.span
            [ Html.Attributes.style "font-weight" "700"
            ]
            [ Html.text <| name ++ ": " ]
        , control
        ]


viewTransformation : Model -> (Object -> Transformation) -> (Transformation -> Msg) -> Html Msg
viewTransformation model acc transformationMsg =
    case getSelectedObject model of
        Just o ->
            transformationView (acc o) transformationMsg

        Nothing ->
            Html.text ""


transformationView : Transformation -> (Transformation -> Msg) -> Html Msg
transformationView transformation transformationMsg =
    let
        msg str =
            case str of
                "Linear" ->
                    transformationMsg (Linear 0)

                "Cyclical" ->
                    transformationMsg (Cyclical { amplitude = 1, frequency = 1 })

                "Random" ->
                    transformationMsg (Random { min = 1, max = 10, seed = Random.initialSeed 0 })

                _ ->
                    NoOp

        linearMsg =
            String.toFloat
                >> Maybe.withDefault 0
                >> Linear
                >> transformationMsg

        renderOption =
            Html.select [ Html.Events.onInput msg ]
                [ Html.option
                    [ Html.Attributes.value "Linear"
                    , Html.Attributes.selected (toOption transformation == "Linear")
                    ]
                    [ Html.text "Linear" ]
                , Html.option
                    [ Html.Attributes.value "Cyclical"
                    , Html.Attributes.selected (toOption transformation == "Cyclical")
                    ]
                    [ Html.text "Cyclical" ]
                , Html.option
                    [ Html.Attributes.value "Random"
                    , Html.Attributes.selected (toOption transformation == "Random")
                    ]
                    [ Html.text "Random" ]
                ]

        amplitudeInput d =
            String.toFloat
                >> Maybe.withDefault 0
                >> (\a -> Cyclical { d | amplitude = a })
                >> transformationMsg

        frequencyInput d =
            String.toFloat
                >> Maybe.withDefault 0
                >> (\a -> Cyclical { d | frequency = a })
                >> transformationMsg

        minInput d =
            String.toFloat
                >> Maybe.withDefault 0
                >> (\a -> Random { d | min = a })
                >> transformationMsg

        maxInput d =
            String.toFloat
                >> Maybe.withDefault 0
                >> (\a -> Random { d | max = a })
                >> transformationMsg

        renderToggler =
            case transformation of
                Linear float ->
                    Html.input
                        [ Html.Attributes.value (String.fromFloat float)
                        , Html.Events.onInput linearMsg
                        ]
                        []

                Cyclical d ->
                    Html.div []
                        [ Html.div []
                            [ Html.label []
                                [ Html.text "Amplitude:"
                                , Html.input
                                    [ Html.Attributes.value (String.fromFloat d.amplitude)
                                    , Html.Events.onInput (amplitudeInput d)
                                    ]
                                    []
                                ]
                            ]
                        , Html.div []
                            [ Html.label []
                                [ Html.text "Frequency:"
                                , Html.input
                                    [ Html.Attributes.value (String.fromFloat d.frequency)
                                    , Html.Events.onInput (frequencyInput d)
                                    ]
                                    []
                                ]
                            ]
                        ]

                Random d ->
                    Html.div []
                        [ Html.div []
                            [ Html.label []
                                [ Html.text "Min:"
                                , Html.input
                                    [ Html.Attributes.value (String.fromFloat d.min)
                                    , Html.Events.onInput (minInput d)
                                    ]
                                    []
                                ]
                            ]
                        , Html.div []
                            [ Html.label []
                                [ Html.text "Max:"
                                , Html.input
                                    [ Html.Attributes.value (String.fromFloat d.max)
                                    , Html.Events.onInput (maxInput d)
                                    ]
                                    []
                                ]
                            ]
                        ]
    in
    Html.div
        []
        [ renderOption
        , renderToggler
        ]


toOption : Transformation -> String
toOption transformation =
    case transformation of
        Linear _ ->
            "Linear"

        Cyclical _ ->
            "Cyclical"

        Random _ ->
            "Random"


canvasWidth =
    1200


canvasHeight =
    900


viewCenterPoint : Bool -> Svg Msg
viewCenterPoint bool =
    if bool then
        Svg.circle
            [ fill "red"
            , r (String.fromFloat <| 3)
            , cx (String.fromFloat <| canvasWidth / 2)
            , cy (String.fromFloat <| canvasHeight / 2)
            ]
            []

    else
        Svg.g [] []


viewObjectSelector : Model -> Html Msg
viewObjectSelector model =
    Dict.toList model.objects
        |> List.map
            (\( id, obj ) ->
                Html.li
                    [ Html.Events.onClick (SelectObject (Just id))
                    , if isSelected model id then
                        Html.Attributes.style "color" "red"

                      else
                        Html.Attributes.style "color" "black"
                    ]
                    [ Html.text <| objectLabel obj ++ " " ++ String.fromInt id ]
            )
        |> Html.ul []


objectLabel : Object -> String
objectLabel object =
    case object.shape of
        Square _ ->
            "Square"

        Circle _ ->
            "Circle"


isSelected : Model -> Id -> Bool
isSelected model id =
    Debug.log "Just Id" (Just id)
        == model.selectedObjectId
        |> Debug.log "EQ"


getSelectedObject : Model -> Maybe Object
getSelectedObject model =
    Maybe.map (\id -> Dict.get id model.objects) model.selectedObjectId
        |> Maybe.join


viewSlider : Model -> String -> (Object -> String) -> (String -> Msg) -> String -> String -> Html Msg
viewSlider model label accessor msg min max =
    case getSelectedObject model of
        Just o ->
            Html.label
                [ Html.Attributes.style "border" "1px solid black"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "margin-bottom" "8px"
                ]
                [ Html.span
                    []
                    [ Html.text <| label ++ ": " ]
                , Html.input
                    [ Html.Attributes.value <| accessor o
                    , Html.Events.onInput msg
                    ]
                    []
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.value <| accessor o
                    , Html.Events.onInput msg
                    , Html.Attributes.min min
                    , Html.Attributes.max max
                    ]
                    []
                ]

        Nothing ->
            Html.span [] []


viewObjects : Model -> Svg Msg
viewObjects model =
    Dict.toList model.objects
        |> List.map (viewObject model)
        |> Svg.g []


viewObject : Model -> ( Int, Object ) -> Svg Msg
viewObject model ( id, object ) =
    let
        shadows =
            List.range 1 (object.loops - 1)
                |> List.map (viewShadow object)
    in
    (renderMain model ( id, object ) :: shadows)
        |> Svg.g []


viewShadow : Object -> Int -> Svg Msg
viewShadow object loop =
    case object.shape of
        Square squareData ->
            Svg.rect
                [ fill "none"
                , stroke "black"
                , transform <| calculatedRotation loop object
                , x (String.fromFloat <| calculatedX loop object)
                , y (String.fromFloat <| calculatedY loop object)
                , width (String.fromFloat <| calculatedWidth loop object)
                , height (String.fromFloat <| calculatedHeight loop object)
                ]
                []

        Circle circleData ->
            Svg.circle
                [ fill "none"
                , stroke "black"
                , transform <| calculatedRotation loop object
                , cx (String.fromFloat <| calculatedX loop object)
                , cy (String.fromFloat <| calculatedY loop object)
                , r (String.fromFloat <| calculatedWidth loop object / 2)
                ]
                []


transformationByLoop : Int -> Transformation -> Float
transformationByLoop loop transformation =
    case transformation of
        Linear float ->
            float * toFloat loop

        Cyclical data ->
            (sin (toFloat loop / data.frequency) + 1) * data.amplitude

        Random data ->
            let
                generator =
                    Random.float data.min data.max

                generated =
                    Random.step generator (Random.initialSeed <| floor (toFloat loop * data.min * data.max))

                fn : Int -> ( Float, Random.Seed ) -> ( Float, Random.Seed )
                fn _ ( _, newSeed ) =
                    Random.step generator newSeed
            in
            List.range 1 loop
                |> List.foldl fn generated
                |> Tuple.first


calculatedX : Int -> Object -> Float
calculatedX loop object =
    case object.shape of
        Square _ ->
            object.x
                - (calculatedWidth loop object / 2)
                + transformationByLoop loop object.xShift

        Circle _ ->
            object.x
                + transformationByLoop loop object.xShift


calculatedY : Int -> Object -> Float
calculatedY loop object =
    case object.shape of
        Square _ ->
            object.y
                - (calculatedHeight loop object / 2)
                + transformationByLoop loop object.yShift

        Circle _ ->
            object.y
                + transformationByLoop loop object.yShift


calculatedWidth : Int -> Object -> Float
calculatedWidth loop object =
    case object.shape of
        Square squareData ->
            squareData.width + transformationByLoop loop object.scale

        Circle circleData ->
            2 * circleData.radius + transformationByLoop loop object.scale


calculatedHeight : Int -> Object -> Float
calculatedHeight loop object =
    case object.shape of
        Square squareData ->
            squareData.height + transformationByLoop loop object.scale

        Circle circleData ->
            2 * circleData.radius + transformationByLoop loop object.scale


calculatedRotation : Int -> Object -> String
calculatedRotation loop object =
    String.concat
        [ "rotate("
        , String.fromFloat <| transformationByLoop loop object.rotation
        , ","
        , String.fromInt object.anchorX
        , ","
        , String.fromInt object.anchorY
        , ")"
        ]


renderMain : Model -> ( Int, Object ) -> Svg Msg
renderMain { guidesVisible, selectedObjectId } ( id, object ) =
    let
        color =
            if selectedObjectId == Just id && guidesVisible then
                "red"

            else
                "black"
    in
    case object.shape of
        Square squareData ->
            Svg.rect
                [ fill "none"
                , stroke color
                , x (String.fromFloat <| calculatedX 0 object)
                , y (String.fromFloat <| calculatedY 0 object)
                , width (String.fromFloat squareData.width)
                , height (String.fromFloat squareData.height)
                , attribute "data-beacon" <| String.fromInt id
                ]
                []

        Circle circleData ->
            Svg.circle
                [ fill "none"
                , stroke color
                , cx (String.fromFloat <| calculatedX 0 object)
                , cy (String.fromFloat <| calculatedY 0 object)
                , r (String.fromFloat <| circleData.radius)
                , attribute "data-beacon" <| String.fromInt id
                ]
                []


toggle : (Bool -> Msg) -> Bool -> String -> Html Msg
toggle msg value text =
    Html.div []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked value
                , Html.Events.onCheck msg
                ]
                []
            , Html.text text
            ]
        ]



-- Json Decoders


decodeDragEvents : Json.Decode.Value -> DragMsg
decodeDragEvents value =
    case Json.Decode.decodeValue msgDecoder value of
        Ok msg ->
            msg

        Err err ->
            -- Crash the javascript :(
            -- I would normally have an error state for the whole
            -- application that maybe this gets set to
            Debug.todo <| Json.Decode.errorToString err


msgDecoder : Json.Decode.Decoder DragMsg
msgDecoder =
    Json.Decode.map3 DragMsg
        (Json.Decode.field "type" eventDecoder)
        (Json.Decode.field "cursor" coordsDecoder)
        (Json.Decode.field "beacons" <| Json.Decode.list draggablesDecoder)


draggablesDecoder : Json.Decode.Decoder ( Id, Coords )
draggablesDecoder =
    Json.Decode.map2
        Tuple.pair
        idDecoder
        coordsDecoder


idDecoder : Json.Decode.Decoder Id
idDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\intString ->
                case String.toInt intString of
                    Just i ->
                        Json.Decode.succeed i

                    Nothing ->
                        Json.Decode.fail "Not a valid Id"
            )
        |> Json.Decode.field "id"


eventDecoder : Json.Decode.Decoder DragEvent
eventDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\eventType ->
                case eventType of
                    "start" ->
                        Json.Decode.succeed Start

                    "move" ->
                        Json.Decode.succeed Move

                    "stop" ->
                        Json.Decode.succeed Stop

                    _ ->
                        Json.Decode.fail ("Unknown drag event type " ++ eventType)
            )


coordsDecoder : Json.Decode.Decoder Coords
coordsDecoder =
    Json.Decode.map2 Coords
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)



-- Functions for manipulating and Coords


closestRect : Coords -> List ( Id, Coords ) -> Maybe Int
closestRect cursor draggables =
    draggables
        |> List.map (Tuple.mapSecond (distance cursor))
        -- Find the vertex closest to the cursor.
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first


distance : Coords -> Coords -> Float
distance coords1 coords2 =
    let
        dx =
            coords1.x - coords2.x

        dy =
            coords1.y - coords2.y
    in
    sqrt ((dx ^ 2) + (dy ^ 2))
