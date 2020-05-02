port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Json.Decode
import Maybe.Extra as Maybe
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, transform, viewBox, width, x, y)


port dragEvents : (Json.Decode.Value -> msg) -> Sub msg


port gotSvg : (String -> msg) -> Sub msg


port getSvg : String -> Cmd msg


type alias Model =
    { objects : Dict Id Object
    , selectedObjectId : Maybe Id
    , currentDraggable : Maybe Id
    }


type alias Id =
    Int


type Msg
    = Drag DragMsg
    | GetSvg
    | GotSvg String
    | SetRotation String
    | SetLoops String
    | SetX String
    | SetY String
    | SetScale String
    | SetXShift String
    | SetYShift String
    | Center
    | Delete
    | SelectObject (Maybe Id)
    | AddShape


type alias Object =
    { x : Float
    , y : Float
    , width : Int
    , height : Int
    , loops : Int
    , scale : Float
    , rotation : Int
    , xShift : Int
    , yShift : Int
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
            [ ( 0, initShape ) ]
    , selectedObjectId = Just 0
    , currentDraggable = Nothing
    }


initShape =
    { x = canvasWidth / 2
    , y = canvasHeight / 2
    , loops = 72
    , rotation = 25
    , width = 10
    , height = 10
    , scale = 2
    , xShift = 0
    , yShift = 0
    }


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

        SetRotation str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | rotation = Maybe.withDefault 0 <| String.toInt str })
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

        SetScale str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | scale = Maybe.withDefault 0 <| String.toFloat str })
                        model.objects
              }
            , Cmd.none
            )

        SetXShift str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | xShift = Maybe.withDefault 0 <| String.toInt str })
                        model.objects
              }
            , Cmd.none
            )

        SetYShift str ->
            ( { model
                | objects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | yShift = Maybe.withDefault 0 <| String.toInt str })
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

        AddShape ->
            let
                ( newSelectedId, newObjects ) =
                    insertShape model.objects
            in
            ( { model | objects = newObjects, selectedObjectId = Just newSelectedId }
            , Cmd.none
            )


download : String -> String -> Cmd msg
download fileName svg =
    Download.string (String.append fileName ".svg") "image/svg+xml" svg


insertShape : Dict Int Object -> ( Int, Dict Int Object )
insertShape dict =
    let
        newKey =
            nextKey dict
    in
    ( newKey, Dict.insert newKey initShape dict )


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
                [ viewObjects model.selectedObjectId model.objects ]
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
                "Rotation"
                (String.fromInt << .rotation)
                SetRotation
                "-180"
                "180"
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
            , viewSlider model
                "Scale"
                (String.fromFloat << .scale)
                SetScale
                "-20"
                "20"
            , viewSlider model
                "X-Shift"
                (String.fromInt << .xShift)
                SetXShift
                "-10"
                "10"
            , viewSlider model
                "Y-Shift"
                (String.fromInt << .yShift)
                SetYShift
                "-10"
                "10"
            , viewObjectSelector model
            , Html.button [ Html.Events.onClick Center ] [ Html.text "Center" ]
            , Html.button [ Html.Events.onClick AddShape ] [ Html.text "Another Square" ]
            , case model.selectedObjectId of
                Just _ ->
                    Html.button [ Html.Events.onClick Delete ] [ Html.text "delete selected" ]

                Nothing ->
                    Html.text ""
            , Html.button [ Html.Events.onClick GetSvg ] [ Html.text "download" ]
            ]
        ]


canvasWidth =
    1200


canvasHeight =
    900


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
                    [ Html.text <| "Square " ++ String.fromInt id ]
            )
        |> Html.ul []


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
            Html.label []
                [ Html.p []
                    [ Html.text <| label ++ ": " ++ accessor o ]
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


viewObjects : Maybe Id -> Dict Id Object -> Svg Msg
viewObjects selectedObjectId objects =
    Dict.toList objects
        |> List.map (viewObject selectedObjectId)
        |> Svg.g []


viewObject : Maybe Id -> ( Int, Object ) -> Svg Msg
viewObject selectedObjectId ( id, object ) =
    let
        shadows =
            List.range 1 (object.loops - 1)
                |> List.map
                    (\o ->
                        Svg.rect
                            [ fill "none"
                            , stroke "black"
                            , transform <| rotation o object
                            , x (String.fromFloat <| object.x + toFloat object.xShift)
                            , y (String.fromFloat <| object.y + toFloat object.yShift)
                            , width (String.fromFloat <| 100 + toFloat o * object.scale)
                            , height (String.fromFloat <| 100 + toFloat o * object.scale)
                            ]
                            []
                    )
    in
    (renderMain selectedObjectId ( id, object ) :: shadows)
        |> Svg.g []


renderMain : Maybe Id -> ( Int, Object ) -> Svg Msg
renderMain maybeSelectedId ( id, object ) =
    let
        color =
            if maybeSelectedId == Just id then
                "red"

            else
                "blue"
    in
    Svg.rect
        [ fill "none"
        , stroke color
        , x (String.fromFloat object.x)
        , y (String.fromFloat object.y)
        , width (String.fromFloat 100)
        , height (String.fromFloat 100)
        , attribute "data-beacon" <| String.fromInt id
        ]
        []



{-
   - rotate(x, x, y)

-}


rotation : Int -> Object -> String
rotation loop obj =
    String.concat
        [ "rotate("
        , String.fromInt <| loop * obj.rotation
        , ", "
        , String.fromFloat <| obj.x + (toFloat obj.width / 2)
        , ", "
        , String.fromFloat <| obj.y + (toFloat obj.width / 2)
        , ")"
        ]


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
