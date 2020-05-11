port module Main exposing (main)

import Base64
import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Dict.Extra
import File.Download as Download
import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Json.Decode
import Json.Encode
import LensHelpers
import Maybe.Extra as Maybe
import Monocle.Compose as Compose
import Monocle.Lens as Lens exposing (Lens)
import Object exposing (Object)
import Random
import Result.Extra
import SelectList exposing (SelectList)
import Shape exposing (Shape(..))
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, transform, viewBox, width, x, y)
import Transformation exposing (Transformation(..))
import Url exposing (Url)


port dragEvents : (Json.Decode.Value -> msg) -> Sub msg


port gotSvg : (String -> msg) -> Sub msg


port getSvg : String -> Cmd msg


port capture : Int -> Cmd msg


type alias Model =
    { objects : Dict Id Object
    , history : SelectList Snapshot
    , selectedObjectId : Maybe Id
    , currentDraggable : Maybe DraggableIdentifier -- TODO get rid of this style of draggable
    , guidesVisible : Bool
    , currentUpdating : ( Maybe Float, Float ) -- Starting Offset
    , navigationKey : Key
    }


type alias Id =
    Int


type Msg
    = Drag DragMsg
    | UrlChanged Url
    | GetSvg
    | GotSvg String
    | SetXShift Transformation
    | SetYShift Transformation
    | SetScale Transformation
    | SetRotation Transformation
    | SetShape Shape
    | SetObject String Object
    | AttributeSlide DragEvent String (Lens Object Float) Pointer.Event
    | AttributeWheel (Lens Object Float) Wheel.Event
    | SetWithLens String (Lens Object Float) Float
    | Center
    | Delete
    | Undo Int
    | SelectObject (Maybe Id)
    | AddNewShape Shape
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


type alias Snapshot =
    { action : Action
    , objectsState : Dict Id Object
    }


type Action
    = Initial
    | AddShape Shape
    | DeleteObject Object
    | ChangeField String
    | ChangeTransformationType Transformation String
    | ChangeShapeType Shape
    | ChangeObject Object String


type DragEvent
    = Start
    | Move
    | Stop


type alias Coords =
    { x : Float, y : Float }


main : Program () Model Msg
main =
    Browser.application
        { init = \_ url key -> ( init key url, Cmd.none )
        , view = \m -> { title = "Plotter Otter", body = [ view m ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map Drag <| dragEvents decodeDragEvents
        , gotSvg GotSvg
        ]


initialSnapshot : Snapshot
initialSnapshot =
    let
        initialObjects =
            Dict.fromList
                [ ( 0
                  , Object.initWithShape
                        canvasWidth
                        canvasHeight
                        Shape.defaultSquare
                  )
                ]
    in
    { action = Initial, objectsState = initialObjects }


objectsFromUrl : Url -> Dict Id Object -> Dict Id Object
objectsFromUrl url default =
    case url.fragment of
        Just base64Fragment ->
            base64Fragment
                |> Base64.decode
                |> Result.map (Json.Decode.decodeString decodeObjects)
                |> Result.map (Result.mapError (always "Couldn't decode from URL"))
                |> Result.Extra.join
                |> Result.withDefault default

        Nothing ->
            default


init : Key -> Url -> Model
init key url =
    { history = SelectList.singleton initialSnapshot
    , objects = objectsFromUrl url initialSnapshot.objectsState
    , selectedObjectId = Just 0
    , currentDraggable = Nothing
    , guidesVisible = True
    , currentUpdating = ( Nothing, 0 )
    , navigationKey = key
    }


svgId =
    "plotter-otter-svg"



-- Update logic


updateObject : Maybe Id -> (Object -> Object) -> Dict Int Object -> Dict Int Object
updateObject maybeId updater objects =
    case maybeId of
        Just selectedId ->
            Dict.update selectedId
                (Maybe.map updater)
                objects

        Nothing ->
            objects


objectsUpdaterNew : Maybe Id -> Lens Object Float -> (Float -> Float) -> Dict Id Object -> Dict Id Object
objectsUpdaterNew maybeId lens modifier objects =
    case maybeId of
        Just selectedId ->
            Dict.update selectedId
                (Maybe.map (Lens.modify lens modifier))
                objects

        Nothing ->
            objects


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AttributeSlide eventType label lens pointerEvent ->
            let
                offset =
                    Tuple.first pointerEvent.pointer.offsetPos
            in
            case eventType of
                Start ->
                    ( { model | currentUpdating = ( Just offset, offset ) }
                    , capture pointerEvent.pointerId
                    )

                Move ->
                    let
                        updated =
                            case model.currentUpdating of
                                ( Just firstValue, lastValue ) ->
                                    let
                                        modNumber =
                                            if pointerEvent.pointer.keys.shift then
                                                10

                                            else
                                                1

                                        modifier f =
                                            if (offset - lastValue) > 0 then
                                                f + modNumber

                                            else
                                                f - modNumber
                                    in
                                    { model
                                        | objects =
                                            objectsUpdaterNew model.selectedObjectId
                                                lens
                                                modifier
                                                model.objects
                                        , currentUpdating = ( Just firstValue, offset )
                                    }

                                ( Nothing, _ ) ->
                                    model
                    in
                    ( updated, Cmd.none )

                Stop ->
                    ( { model
                        | currentUpdating = ( Nothing, 0 )
                        , history =
                            appendHistory
                                { action = ChangeField label, objectsState = model.objects }
                                model.history
                      }
                    , saveToUrl model.navigationKey model.objects
                    )

        AttributeWheel lens wheelEvent ->
            ( { model
                | objects =
                    objectsUpdaterNew model.selectedObjectId
                        lens
                        ((+) wheelEvent.deltaY)
                        model.objects
              }
            , Cmd.none
            )

        SetWithLens label lens val ->
            let
                newObjects =
                    objectsUpdaterNew model.selectedObjectId
                        lens
                        (always val)
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = ChangeField label, objectsState = newObjects }
                        model.history
              }
            , saveToUrl model.navigationKey model.objects
            )

        Undo int ->
            let
                movedHistory =
                    SelectList.selectBy int model.history
                        |> Maybe.withDefault model.history
            in
            ( { model
                | objects = .objectsState (SelectList.selected movedHistory)
                , history = movedHistory
              }
            , Cmd.none
            )

        UrlChanged url ->
            ( model, Cmd.none )

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
                                    model

                                -- { model
                                --     | objects =
                                --         objectsUpdater
                                --             model.selectedObjectId
                                --             (attributeToLens attr)
                                --             (String.fromFloat (cursor.x - coords.x))
                                --             model.objects
                                -- }
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
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | rotation = transformation })
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = ChangeTransformationType transformation "Rotation"
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetShape shape ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | shape = shape })
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = ChangeShapeType shape
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetObject label object ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (always object)
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = ChangeObject object label
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetXShift transformation ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | xShift = transformation })
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action =
                            ChangeTransformationType transformation "X Shift"
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetYShift transformation ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | yShift = transformation })
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action =
                            ChangeTransformationType transformation "Y Shift"
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetScale transformation ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (\x -> { x | scale = transformation })
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action =
                            ChangeTransformationType transformation "Scale"
                        , objectsState = newObjects
                        }
                        model.history
              }
            , saveToUrl model.navigationKey newObjects
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
                ( removedObject, newObjects ) =
                    case model.selectedObjectId of
                        Just selectedId ->
                            case Dict.get selectedId model.objects of
                                Just obj ->
                                    ( Just obj, Dict.remove selectedId model.objects )

                                Nothing ->
                                    ( Nothing, model.objects )

                        Nothing ->
                            ( Nothing, model.objects )

                newModel =
                    case removedObject of
                        Just removed ->
                            { model
                                | selectedObjectId = getAnyId newObjects
                                , objects = newObjects
                                , history =
                                    appendHistory
                                        { action = DeleteObject removed
                                        , objectsState = newObjects
                                        }
                                        model.history
                            }

                        Nothing ->
                            model
            in
            ( newModel, saveToUrl model.navigationKey newModel.objects )

        SelectObject maybeObjectId ->
            ( { model | selectedObjectId = maybeObjectId }
            , Cmd.none
            )

        AddNewShape newShape ->
            let
                ( newSelectedId, newObjects ) =
                    insertShape newShape model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = AddShape newShape, objectsState = newObjects }
                        model.history
                , selectedObjectId = Just newSelectedId
              }
            , saveToUrl model.navigationKey newObjects
            )

        SetGuidesVisible bool ->
            ( { model | guidesVisible = bool }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


saveToUrl : Key -> Dict Id Object -> Cmd Msg
saveToUrl key objects =
    objects
        |> encodeObjects
        |> Json.Encode.encode 0
        |> Base64.encode
        |> String.append "#"
        |> Browser.Navigation.replaceUrl key


appendHistory : Snapshot -> SelectList Snapshot -> SelectList Snapshot
appendHistory newSnapshot history =
    history
        |> SelectList.updateBefore (always [])
        |> SelectList.insertAfter newSnapshot


getAnyId : Dict comparable b -> Maybe comparable
getAnyId dict =
    dict
        |> Dict.keys
        |> List.head


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


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.id "main"
        , Html.Attributes.class "flex p-4 h-full"
        ]
        [ Html.div []
            [ Html.div
                [ Html.Attributes.class "border-black border-4"
                , Html.Attributes.id svgId
                , Html.Attributes.style "width" (String.fromInt canvasWidth ++ "px")
                , Html.Attributes.style "height" (String.fromInt canvasHeight ++ "px")
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
            , Html.div [ Html.Attributes.class "flex pt-4 pr-4 justify-between" ]
                [ Html.h1 [ Html.Attributes.class "border-t-4 inline-block" ]
                    [ Html.text "Plotter Otter" ]
                , Html.div [ Html.Attributes.class "flex" ]
                    [ Html.div [ Html.Attributes.class "pr-4" ] [ button Secondary (Undo 1) "Undo" ]
                    , Html.div [ Html.Attributes.class "" ] [ button Primary GetSvg "Download" ]

                    -- , Html.text <| Base64.encode (Json.Encode.encode 0 (encodeObjects model.objects))
                    ]
                ]
            ]
        , Html.div
            [ Html.Attributes.class "flex-col space-y-4 pl-4 h-full w-1/5 overflow-scroll" ]
            [ controlContainer <|
                [ controlSection "Shape Attributes"
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "X"
                            , lens = Object.xLens
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Y"
                            , lens = Object.yLens
                            }
                    ]
                , withSelectedObject model emptyHtml <|
                    sizeAttributes
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Rotate"
                            , lens = Object.baseRotation
                            }
                    , withSelectedObject model emptyHtml <|
                        viewShapeConverters
                    ]
                , toggle SetGuidesVisible model.guidesVisible "Show Guides"
                , controlSection "Transformations"
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Loops"
                            , lens = Object.loopFloorLens
                            }
                    ]
                , controlSubSection "Rotation"
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Anchor X"
                            , lens = Object.anchorXLens
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Anchor Y"
                            , lens = Object.anchorYLens
                            }
                    ]
                , viewTransformation model Object.rotationLens .rotation SetRotation
                , controlSubSection "X-Shift"
                , viewTransformation model Object.xShiftLens .xShift SetXShift
                , controlSubSection "Y-Shift"
                , viewTransformation model Object.yShiftLens .yShift SetYShift
                , controlSubSection "Scale"
                , viewTransformation model Object.scaleLens .scale SetScale
                ]
            , controlContainer <|
                [ controlSection "History"
                , viewHistory model.history
                ]
            , controlContainer <|
                [ controlSection "Shapes"
                , viewObjectSelector model
                , controlRow <|
                    [ button Secondary (AddNewShape Shape.defaultSquare) "+ ⬛️"
                    , button Secondary (AddNewShape Shape.defaultCircle) "+ ⚫️"
                    ]
                , case model.selectedObjectId of
                    Just _ ->
                        button Caution Delete "Delete Selected"

                    Nothing ->
                        Html.text ""
                ]
            , controlContainer <|
                [ controlSection "Examples"
                , viewExamples
                ]
            ]
        ]


viewExamples : Html Msg
viewExamples =
    let
        asItem ( label, obj ) =
            { text = label
            , clickHandler = \i -> SetObject label obj
            , itemState = Normal
            }
    in
    Object.examples canvasWidth canvasHeight
        |> List.map asItem
        |> itemList


viewShapeConverters : Object -> Html Msg
viewShapeConverters object =
    case object.shape of
        Square _ ->
            button Secondary (SetShape <| Shape.toCircle object.shape) "⚫️"

        Circle _ ->
            button Secondary (SetShape <| Shape.toSquare object.shape) "⬛️"


viewHistory : SelectList Snapshot -> Html Msg
viewHistory history =
    history
        |> SelectList.map (toItem (SelectList.index history))
        |> SelectList.updateSelected (\a -> { a | itemState = Highlighted })
        |> SelectList.updateBefore (\before -> List.map (\a -> { a | itemState = Faded }) before)
        |> SelectList.toList
        |> itemList
        |> List.singleton
        |> Html.div [ Html.Attributes.style "height" "150px" ]


toItem : Int -> Snapshot -> Item
toItem selectedIndex snapshot =
    { text = actionLabel snapshot.action
    , clickHandler = \i -> Undo (i - selectedIndex)
    , itemState = Normal
    }


type alias Item =
    { text : String
    , clickHandler : Int -> Msg
    , itemState : ItemState
    }


type ItemState
    = Normal
    | Highlighted
    | Faded


itemList : List Item -> Html Msg
itemList items =
    let
        itemClassList itemState =
            [ ( "border-b pl-2 hover:bg-black hover:text-white cursor-pointer", True )
            , case itemState of
                Highlighted ->
                    ( "bg-purple-200", True )

                Faded ->
                    ( "bg-orange-200", True )

                _ ->
                    ( "", False )
            ]
    in
    Html.ul [ Html.Attributes.class "border max-h-sm overflow-scroll" ] <|
        List.indexedMap
            (\int item ->
                Html.li
                    [ Html.Attributes.classList (itemClassList item.itemState)
                    , Html.Events.onClick (item.clickHandler int)
                    ]
                    [ Html.text <| item.text ]
            )
            items


actionLabel : Action -> String
actionLabel action =
    case action of
        Initial ->
            "Initial"

        AddShape s ->
            "Added shape"

        DeleteObject o ->
            "Deleted shape"

        ChangeField s ->
            "Updated" ++ " " ++ s

        ChangeTransformationType t label ->
            String.concat
                [ "Switched "
                , label
                , " to "
                , Transformation.label t
                ]

        ChangeShapeType shape ->
            String.concat
                [ "Switched to "
                , Shape.label shape
                ]

        ChangeObject object label ->
            String.concat [ "Showing ", label, " example" ]


type Level
    = Primary
    | Secondary
    | Caution


button : Level -> Msg -> String -> Html Msg
button level msg text =
    let
        colors =
            case level of
                Primary ->
                    "bg-purple-400 hover:bg-purple-200 text-black"

                Secondary ->
                    "border border-purple-300 hover:bg-purple-200 text-black"

                Caution ->
                    "border border-orange-300 hover:bg-orange-200 text-black"

        classList =
            [ ( "font-bold py-2 px-4 align-center rounded", True )
            , ( colors, True )
            ]

        -- "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
    in
    Html.button
        [ Html.Attributes.classList classList
        , Html.Events.onClick msg
        ]
        [ Html.text text ]


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
    , lens : Lens Object Float
    }


sizeAttributes : Object -> Html Msg
sizeAttributes object =
    case object.shape of
        Circle cd ->
            controlRow <|
                [ numberInput
                    { label = "Radius"
                    , lens = Lens.compose Object.shapeLens (Shape.radiusLens 10)
                    }
                    object
                ]

        Square sd ->
            controlRow <|
                [ numberInput
                    { label = "Width"
                    , lens = Lens.compose Object.shapeLens (Shape.widthLens 10)
                    }
                    object
                , numberInput
                    { label = "Height"
                    , lens = Lens.compose Object.shapeLens (Shape.heightLens 10)
                    }
                    object
                ]


numberInput : NumberInputConfig -> Object -> Html Msg
numberInput { label, lens } object =
    let
        inputMsg str =
            Maybe.withDefault (.get lens object) (String.toFloat str)
                |> SetWithLens label lens

        downEvent =
            Html.Events.custom "pointerdown" <|
                Json.Decode.map
                    (\msg -> { message = msg, stopPropagation = True, preventDefault = False })
                    (Json.Decode.map (AttributeSlide Start label lens) Pointer.eventDecoder)
    in
    Html.label
        [ Html.Attributes.style "user-select" "none"
        , Html.Attributes.class "flex justify-between pr-4 cursor-ew"
        , Pointer.onMove <| AttributeSlide Move label lens
        , Pointer.onUp <| AttributeSlide Stop label lens
        , downEvent
        , Wheel.onWheel <| AttributeWheel lens
        ]
        [ Html.div [] [ Html.text <| label ++ ":" ]
        , Html.div [ Html.Attributes.class "pr-4" ]
            [ Html.input
                [ Html.Attributes.class "border border-grey hover:border-black w-12 text-right cursor-ew"
                , Html.Attributes.value <| String.fromFloat <| .get lens object
                , Html.Attributes.type_ "number"
                , Html.Events.onInput inputMsg
                ]
                []
            ]
        ]


controlContainer : List (Html Msg) -> Html Msg
controlContainer controls =
    Html.div
        [ Html.Attributes.class "border-solid border-4 border-black p-2 max-w-md text-sm"
        ]
        [ Html.div [ Html.Attributes.class "flex-col space-y-2 pt-2" ] controls
        ]


controlRow : List (Html Msg) -> Html Msg
controlRow controls =
    Html.div [ Html.Attributes.class "grid grid-cols-2" ] <|
        List.map (\c -> Html.div [ Html.Attributes.class "grid-span-1" ] [ c ]) controls


controlSection : String -> Html Msg
controlSection name =
    Html.h3 [ Html.Attributes.class "border-b-2" ] [ Html.text name ]


controlSubSection : String -> Html Msg
controlSubSection name =
    Html.h4 [ Html.Attributes.class "pt-4" ] [ Html.text name ]


viewTransformation : Model -> Lens Object Transformation -> (Object -> Transformation) -> (Transformation -> Msg) -> Html Msg
viewTransformation model lens acc transformationMsg =
    case getSelectedObject model of
        Just o ->
            transformationView (acc o) lens transformationMsg o

        Nothing ->
            Html.text ""


type alias TabOption =
    { name : String
    , selected : Bool
    , clickMsg : Msg
    }


tabbed : List TabOption -> Html Msg
tabbed options =
    Html.div [ Html.Attributes.class "flex border-b" ] <|
        List.map
            (\opt ->
                let
                    outerClassList =
                        [ ( "mr-1 cursor-pointer", True )
                        , ( "-mb-px", opt.selected )
                        ]

                    innerClassList =
                        [ ( "inline-block px-2", True )
                        , ( "bg-white border-l border-t border-r font-bold", opt.selected )
                        ]
                in
                Html.div
                    [ Html.Attributes.classList outerClassList
                    ]
                    [ Html.button
                        [ Html.Attributes.classList innerClassList
                        , Html.Events.onClick opt.clickMsg
                        ]
                        [ Html.text opt.name ]
                    ]
            )
            options


transformationView : Transformation -> Lens Object Transformation -> (Transformation -> Msg) -> Object -> Html Msg
transformationView transformation lens transformationMsg object =
    let
        transformationEq t =
            t == toOption transformation

        tabOptions =
            [ TabOption "Linear"
                (transformationEq "Linear")
                (transformationMsg (Linear 0))
            , TabOption "Cyclical"
                (transformationEq "Cyclical")
                (transformationMsg (Cyclical { amplitude = 1, frequency = 1 }))
            , TabOption "Random"
                (transformationEq "Random")
                (transformationMsg (Random { min = 0, max = 100, seed = 0 }))
            ]

        renderToggler =
            case transformation of
                Linear float ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Number"
                            , lens = Lens.compose lens (Transformation.linearLens 0)
                            }
                            object
                        ]
                    ]

                Cyclical d ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Amplitude"
                            , lens = Lens.compose lens (Transformation.amplitudeLens 0)
                            }
                            object
                        , numberInput
                            { label = "Frequency"
                            , lens = Lens.compose lens (Transformation.frequencyLens 0)
                            }
                            object
                        ]
                    ]

                Random d ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Min"
                            , lens = Lens.compose lens (Transformation.minLens 0)
                            }
                            object
                        , numberInput
                            { label = "Max"
                            , lens = Lens.compose lens (Transformation.maxLens 0)
                            }
                            object
                        ]
                    , controlRow <|
                        [ numberInput
                            { label = "Rand"
                            , lens = Lens.compose lens (Transformation.randLens 0)
                            }
                            object
                        ]
                    ]
    in
    Html.div []
        [ tabbed tabOptions
        , Html.div [ Html.Attributes.class "border-l border-r border-b p-2" ]
            renderToggler
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
    let
        asItem ( id, obj ) =
            { text = objectLabel obj ++ " " ++ String.fromInt id
            , clickHandler = always (SelectObject (Just id))
            , itemState =
                if isSelected model id then
                    Highlighted

                else
                    Normal
            }
    in
    Dict.toList model.objects
        |> List.map asItem
        |> itemList



--     (\( id, obj ) ->
--         Html.li
--             [ Html.Events.onClick (SelectObject (Just id))
--             , if isSelected model id then
--                 Html.Attributes.style "color" "red"
--               else
--                 Html.Attributes.style "color" "black"
--             ]
--             [ Html.text <| objectLabel obj ++ " " ++ String.fromInt id ]
--     )
-- |> Html.ul []


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


loopToColor : Model -> Int -> Int -> String
loopToColor model id loop =
    if model.guidesVisible && loop == 1 && isSelected model id then
        "red"

    else
        "black"


viewObjects : Model -> Svg Msg
viewObjects model =
    Dict.toList model.objects
        |> List.map (viewObject model)
        |> Svg.g []


viewObject : Model -> ( Int, Object ) -> Svg Msg
viewObject model ( id, object ) =
    List.range 1 object.loops
        |> List.map (viewShadow model id object)
        |> Svg.g []


viewShadow : Model -> Int -> Object -> Int -> Svg Msg
viewShadow model id object loop =
    case object.shape of
        Square squareData ->
            Svg.rect
                [ fill "none"
                , stroke <| loopToColor model id loop
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
                , stroke <| loopToColor model id loop
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
                    Random.step generator (Random.initialSeed <| floor data.seed)

                fn : Int -> ( Float, Random.Seed ) -> ( Float, Random.Seed )
                fn _ ( _, newSeed ) =
                    Random.step generator newSeed
            in
            List.range 0 loop
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
        , object.rotation
            |> transformationByLoop loop
            |> (+) object.baseRotation
            |> String.fromFloat
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
            , Html.span
                [ Html.Attributes.class "pl-2 cursor-pointer" ]
                [ Html.text text ]
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



-- attributeToLens : Attribute -> Lens Object String
-- attributeToLens attr =
--     case attr of
--         Loops ->
--             Object.loopsLens
--         X ->
--             Object.xLens
--         Y ->
--             Object.yLens
--         AnchorX ->
--             Object.anchorXLens
--         AnchorY ->
--             Object.anchorYLens


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


decodeObjects : Json.Decode.Decoder (Dict Id Object)
decodeObjects =
    Json.Decode.dict Object.decoder
        |> Json.Decode.map (Dict.Extra.mapKeys (Maybe.withDefault 0 << String.toInt))


encodeObjects : Dict Id Object -> Json.Encode.Value
encodeObjects objectDict =
    Json.Encode.dict
        String.fromInt
        Object.encode
        objectDict
