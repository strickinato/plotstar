port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Json.Decode
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
    | SetLoops Id String
    | SelectObject (Maybe Id)
    | AddShape


type alias Object =
    { x : Float
    , y : Float
    , loops : Int
    , rotation : Int
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
            [ ( 0, { x = 200, y = 200, loops = 100, rotation = 1 } ) ]
    , selectedObjectId = Just 0
    , currentDraggable = Nothing
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

        SetRotation string ->
            let
                newObjects =
                    case model.selectedObjectId of
                        Just selectedId ->
                            updateRotation selectedId
                                (Maybe.withDefault 0 (String.toInt string))
                                model.objects

                        Nothing ->
                            model.objects
            in
            ( { model | objects = newObjects }
            , Cmd.none
            )

        SetLoops id string ->
            ( { model
                | objects =
                    updateLoops id
                        (Maybe.withDefault 0 (String.toInt string))
                        model.objects
              }
            , Cmd.none
            )

        SelectObject maybeObjectId ->
            ( { model | selectedObjectId = maybeObjectId }
            , Cmd.none
            )

        AddShape ->
            ( { model | objects = insertShape model.objects }
            , Cmd.none
            )


download : String -> String -> Cmd msg
download fileName svg =
    Download.string (String.append fileName ".svg") "image/svg+xml" svg


insertShape : Dict Int Object -> Dict Int Object
insertShape dict =
    Dict.insert (Dict.size dict)
        { x = 200, y = 200, loops = 100, rotation = 1 }
        dict


updateRotation : Id -> Int -> Dict Int Object -> Dict Int Object
updateRotation id newRotation dict =
    Dict.update id
        (Maybe.map
            (\a -> { a | rotation = newRotation })
        )
        dict


updateLoops : Id -> Int -> Dict Int Object -> Dict Int Object
updateLoops id newLoops dict =
    Dict.update id
        (Maybe.map
            (\a -> { a | loops = newLoops })
        )
        dict


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
        ]
        [ Html.div
            [ Html.Attributes.style "border" "2px solid black"
            , Html.Attributes.id svgId
            ]
            [ Svg.svg
                [ Html.Attributes.style "width" "600px"
                , Html.Attributes.style "height" "600px"
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
            , viewSlider "Loops"
                .loops
                (SetLoops 0)
                (Dict.get 0 model.objects)
                "1"
                "360"
            , viewSlider "Rotation"
                .rotation
                SetRotation
                (Dict.get 0 model.objects)
                "-180"
                "180"
            , viewObjectSelector model
            , Html.button [ Html.Events.onClick AddShape ] [ Html.text "Another Square" ]
            , Html.button [ Html.Events.onClick GetSvg ] [ Html.text "download" ]
            ]
        ]


viewObjectSelector : Model -> Html Msg
viewObjectSelector model =
    Dict.toList model.objects
        |> List.map
            (\( id, obj ) ->
                Html.li [ Html.Events.onClick (SelectObject (Just id)) ]
                    [ Html.text <| "Square " ++ String.fromInt id ]
            )
        |> Html.ul []


viewSlider : String -> (Object -> Int) -> (String -> Msg) -> Maybe Object -> String -> String -> Html Msg
viewSlider label acc msg obj min max =
    case obj of
        Just o ->
            Html.label []
                [ Html.p []
                    [ Html.text label ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.value <| (String.fromInt << acc) o
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
                            , transform <| rotation (o * object.rotation)
                            , x (String.fromFloat object.x)
                            , y (String.fromFloat object.y)
                            , width (String.fromFloat 100)
                            , height (String.fromFloat 100)
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


rotation : Int -> String
rotation int =
    String.concat
        [ "rotate("
        , String.fromInt int
        , ", 500, 500)"
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
