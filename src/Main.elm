port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Json.Decode
import Maybe.Extra as Maybe
import Object exposing (Object)
import Random
import Shape exposing (Shape(..))
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, transform, viewBox, width, x, y)
import Transformation exposing (Transformation(..))


port dragEvents : (Json.Decode.Value -> msg) -> Sub msg


port gotSvg : (String -> msg) -> Sub msg


port getSvg : String -> Cmd msg


type alias Model =
    { objects : Dict Id Object
    , selectedObjectId : Maybe Id
    , currentDraggable : Maybe DraggableIdentifier
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


type alias DragMsg =
    { event : DragEvent
    , cursor : Coords
    , draggables : List ( DraggableType, Coords )
    }


type DraggableType
    = ShapeDraggable Id
    | AttributeDraggable Attribute


type alias DraggableIdentifier =
    { id : DraggableType
    , dragStart : Coords
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
            [ ( 0
              , Object.initWithShape
                    canvasWidth
                    canvasHeight
                    Shape.defaultCircle
              )
            ]
    , selectedObjectId = Just 0
    , currentDraggable = Nothing
    , guidesVisible = True
    }


svgId =
    "plotter-otter-svg"



-- Update logic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Current Draggable " model.currentDraggable
    in
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
                                                distance cursor draggable < 80
                                            )
                                        |> closestIdentifier cursor
                                        |> Maybe.map (\identifier -> Just { id = identifier, dragStart = cursor })
                                        |> Maybe.join
                            }

                        Move ->
                            case Maybe.map (\a -> Tuple.pair a.id a.dragStart) model.currentDraggable of
                                Just ( ShapeDraggable id, _ ) ->
                                    moveDraggable model cursor id

                                Just ( AttributeDraggable attr, coords ) ->
                                    { model
                                        | objects =
                                            updateObject
                                                model.selectedObjectId
                                                (attributeUpdater attr (cursor.x - coords.x))
                                                model.objects
                                    }

                                Nothing ->
                                    model

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
                        (\x -> { x | anchorX = Maybe.withDefault 0 <| String.toFloat str })
                        model.objects
              }
            , Cmd.none
            )

        SetAnchorY str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | anchorY = Maybe.withDefault 0 <| String.toFloat str })
                        model.objects
              }
            , Cmd.none
            )

        Center ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | x = toFloat canvasWidth / 2, y = toFloat canvasHeight / 2 })
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
                    insertShape Shape.defaultSquare model.objects
            in
            ( { model | objects = newObjects, selectedObjectId = Just newSelectedId }
            , Cmd.none
            )

        AddCircle ->
            let
                ( newSelectedId, newObjects ) =
                    insertShape Shape.defaultCircle model.objects
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
    ( newKey
    , Dict.insert newKey
        (Object.initWithShape canvasWidth canvasHeight shape)
        dict
    )


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
                , viewAnchorPoint model
                , viewObjects model
                ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "padding-left" "24px"
            ]
            [ Html.h3 [] [ Html.text "Shape Attributes" ]
            , controlContainer "Shadow Repititions" <|
                withSelectedObject model emptyHtml <|
                    numberInput
                        { label = "Number"
                        , getValue = .loops >> toFloat
                        , forAttribute = Loops
                        }
            , controlContainer "Position" <|
                Html.div []
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "X"
                            , getValue = .x
                            , forAttribute = X
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Y"
                            , getValue = .y
                            , forAttribute = Y
                            }
                    ]
            , Html.h3 [] [ Html.text "Loop Transformations" ]
            , controlContainer "Rotation" <|
                Html.div []
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "AnchorX"
                            , getValue = .anchorX
                            , forAttribute = AnchorX
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "AnchorY"
                            , getValue = .anchorY
                            , forAttribute = AnchorY
                            }
                    , viewTransformation model .rotation SetRotation
                    ]
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
            , Html.h3 [] [ Html.text "Actions" ]
            , toggle SetGuidesVisible model.guidesVisible "Show Guides"
            , Html.button [ Html.Events.onClick GetSvg ] [ Html.text "download" ]
            , Html.h3 [] [ Html.text "Shapes" ]
            , viewObjectSelector model
            , Html.button [ Html.Events.onClick AddSquare ] [ Html.text "Another Square" ]
            , Html.button [ Html.Events.onClick AddCircle ] [ Html.text "Another Circle" ]
            , case model.selectedObjectId of
                Just _ ->
                    Html.button [ Html.Events.onClick Delete ] [ Html.text "delete selected" ]

                Nothing ->
                    Html.text ""
            ]
        ]


emptyHtml : Html Msg
emptyHtml =
    Html.span [] []


withSelectedObject : Model -> a -> (Object -> a) -> a
withSelectedObject model default fn =
    case getSelectedObject model of
        Just a ->
            fn a

        Nothing ->
            default


type alias NumberInputConfig =
    { label : String
    , getValue : Object -> Float
    , forAttribute : Attribute
    }


numberInput : NumberInputConfig -> Object -> Html Msg
numberInput { label, getValue, forAttribute } object =
    Html.div []
        [ Html.label
            [ Html.Attributes.style "cursor" "ew-resize"
            , Html.Attributes.style "user-select" "none"
            , attribute "data-beacon" <| attributeToId forAttribute
            ]
            [ Html.text <| label ++ ":"
            , Html.input
                [ Html.Attributes.value <| String.fromFloat (getValue object)
                , Html.Attributes.type_ "number"
                ]
                []
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
        renderRadioOptions =
            Html.div []
                [ Html.input
                    [ Html.Attributes.value "Linear"
                    , Html.Attributes.checked (toOption transformation == "Linear")
                    , Html.Attributes.type_ "Radio"
                    , Html.Events.onCheck (always <| transformationMsg (Linear 0))
                    ]
                    []
                , Html.label [] [ Html.text "Linear" ]
                , Html.input
                    [ Html.Attributes.value "Cyclical"
                    , Html.Attributes.checked (toOption transformation == "Cyclical")
                    , Html.Attributes.type_ "Radio"
                    , Html.Events.onCheck
                        (always <| transformationMsg (Cyclical { amplitude = 1, frequency = 1 }))
                    ]
                    []
                , Html.label [] [ Html.text "Cyclical" ]
                , Html.input
                    [ Html.Attributes.value "Random"
                    , Html.Attributes.checked (toOption transformation == "Random")
                    , Html.Attributes.type_ "Radio"
                    , Html.Events.onCheck
                        (always <| transformationMsg (Random { min = 1, max = 10, seed = Random.initialSeed 0 }))
                    ]
                    []
                , Html.label [] [ Html.text "Random" ]
                ]

        linearMsg =
            String.toFloat
                >> Maybe.withDefault 0
                >> Linear
                >> transformationMsg

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
        [ renderRadioOptions
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


canvasWidth : Int
canvasWidth =
    1200


canvasHeight : Int
canvasHeight =
    900


viewCenterPoint : Bool -> Svg Msg
viewCenterPoint bool =
    if bool then
        Svg.circle
            [ fill "red"
            , r (String.fromFloat <| 3)
            , cx (String.fromInt <| canvasWidth // 2)
            , cy (String.fromInt <| canvasHeight // 2)
            ]
            []

    else
        Svg.g [] []


viewAnchorPoint : Model -> Svg Msg
viewAnchorPoint model =
    case ( model.guidesVisible, getSelectedObject model ) of
        ( True, Just obj ) ->
            Svg.circle
                [ fill "red"
                , r (String.fromInt <| 2)
                , cx (String.fromFloat obj.anchorX)
                , cy (String.fromFloat obj.anchorY)
                ]
                []

        _ ->
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
    Just id == model.selectedObjectId


getSelectedObject : Model -> Maybe Object
getSelectedObject model =
    Maybe.map (\id -> Dict.get id model.objects) model.selectedObjectId
        |> Maybe.join


viewSlider : Model -> String -> (Object -> String) -> (String -> Msg) -> String -> String -> Html Msg
viewSlider model label accessor msg min max =
    case getSelectedObject model of
        Just o ->
            Html.label []
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
        , String.fromFloat object.anchorX
        , ","
        , String.fromFloat object.anchorY
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


draggablesDecoder : Json.Decode.Decoder ( DraggableType, Coords )
draggablesDecoder =
    Json.Decode.map2
        Tuple.pair
        idDecoder
        coordsDecoder


idDecoder : Json.Decode.Decoder DraggableType
idDecoder =
    Json.Decode.string
        |> Json.Decode.andThen idToType
        |> Json.Decode.field "id"



-- ATTRIBUTE


type Attribute
    = Loops
    | X
    | Y
    | AnchorX
    | AnchorY


idToType : String -> Json.Decode.Decoder DraggableType
idToType str =
    case String.toInt str of
        Just i ->
            Json.Decode.succeed <| ShapeDraggable i

        Nothing ->
            case idToAttribute str of
                Ok id ->
                    Json.Decode.succeed id

                Err err ->
                    Json.Decode.fail "Not a valid Id"


attributeToId : Attribute -> String
attributeToId attr =
    case attr of
        Loops ->
            "loops"

        X ->
            "x"

        Y ->
            "y"

        AnchorX ->
            "anchor-x"

        AnchorY ->
            "anchor-y"


idToAttribute : String -> Result String DraggableType
idToAttribute str =
    case str of
        "loops" ->
            Ok <| AttributeDraggable Loops

        "x" ->
            Ok <| AttributeDraggable X

        "y" ->
            Ok <| AttributeDraggable Y

        "anchor-x" ->
            Ok <| AttributeDraggable AnchorX

        "anchor-y" ->
            Ok <| AttributeDraggable AnchorY

        other ->
            Err <| other ++ " is not a thing"


attributeUpdater : Attribute -> Float -> Object -> Object
attributeUpdater attr value obj =
    case attr of
        Loops ->
            { obj | loops = floor value }

        X ->
            { obj | x = toFloat canvasWidth / 2 + value }

        Y ->
            { obj | y = toFloat canvasHeight / 2 + value }

        AnchorX ->
            { obj | anchorX = toFloat canvasWidth / 2 + value }

        AnchorY ->
            { obj | anchorY = toFloat canvasHeight / 2 + value }


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


closestIdentifier : Coords -> List ( a, Coords ) -> Maybe a
closestIdentifier cursor draggables =
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
