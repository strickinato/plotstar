port module Main exposing (main)

import Base64
import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
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
import ObjectDict exposing (ObjectDict)
import Process
import Random
import SelectList exposing (SelectList)
import Shape exposing (Shape(..))
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, transform, viewBox, width, x, y)
import Task
import Time
import Transformation exposing (Transformation(..))
import Url exposing (Url)


type alias Model =
    { objects : ObjectDict
    , history : SelectList Snapshot
    , selectedObjectId : Maybe ObjectDict.Id
    , guidesVisible : Bool
    , currentUpdating : ( Maybe Float, Float ) -- Starting Offset
    , wheelState : Dict String Time.Posix
    , navigationKey : Key
    }


canvasWidth : Int
canvasWidth =
    1200


canvasHeight : Int
canvasHeight =
    900


type Msg
    = UrlChanged Url
    | GetSvg
    | GotSvg String
    | DebounceTime Action String Time.Posix
    | CheckDebounce Action Time.Posix String ()
    | SetShape Shape
    | SetObject String Object
    | AttributeSlide DragEvent String (Lens Object Float) Pointer.Event
    | AttributeWheel Action String (Lens Object Float) Wheel.Event
    | SetWithLens Action (Lens Object Float) Float
    | SetTransformation String (Lens Object Transformation) Transformation
    | Center
    | Delete
    | Undo Int
    | SelectObject (Maybe ObjectDict.Id)
    | AddNewShape Shape
    | SetGuidesVisible Bool
    | NoOp


type alias Snapshot =
    { action : Action
    , objectsState : ObjectDict
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


type alias Flags =
    { timestamp : Float }


main : Program Flags Model Msg
main =
    Browser.application
        { init = \flags url key -> ( init flags key url, Cmd.none )
        , view = \m -> { title = "Plotter Otter", body = [ view m ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ gotSvg GotSvg ]


defaultObjects : ObjectDict
defaultObjects =
    Dict.fromList
        [ ( 0
          , Object.initWithShape
                canvasWidth
                canvasHeight
                Shape.defaultSquare
          )
        ]


init : Flags -> Key -> Url -> Model
init flags key url =
    let
        randomStart =
            Dict.fromList
                [ ( 0
                  , Object.randomExample
                        canvasWidth
                        canvasHeight
                        flags.timestamp
                  )
                ]

        objectsFromUrl =
            url.fragment
                |> Maybe.map (ObjectDict.fromBase64 >> Result.withDefault defaultObjects)
                |> Maybe.withDefault randomStart
    in
    { history =
        SelectList.singleton
            { action = Initial, objectsState = objectsFromUrl }
    , objects = objectsFromUrl
    , selectedObjectId = Just 0
    , guidesVisible = False
    , currentUpdating = ( Nothing, 0 )
    , wheelState = Dict.empty
    , navigationKey = key
    }


svgId =
    "plotter-otter-svg"


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
                                            updateObject model.selectedObjectId
                                                (Lens.modify lens modifier)
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

        AttributeWheel action id lens wheelEvent ->
            let
                runDebounce =
                    Task.perform (DebounceTime action id) Time.now
            in
            ( { model
                | objects =
                    updateObject model.selectedObjectId
                        (Lens.modify lens ((+) wheelEvent.deltaY))
                        model.objects
              }
            , runDebounce
            )

        SetWithLens action lens val ->
            let
                newObjects =
                    updateObject model.selectedObjectId
                        (Lens.modify lens (always val))
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = action, objectsState = newObjects }
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

        GetSvg ->
            ( model, getSvg svgId )

        GotSvg output ->
            ( model, download "your-svg" output )

        DebounceTime action id posix ->
            ( { model
                | wheelState =
                    Dict.insert id
                        posix
                        model.wheelState
              }
            , Task.perform (CheckDebounce action posix id) (Process.sleep 800)
            )

        CheckDebounce action oldTime id _ ->
            case Dict.get id model.wheelState of
                Just lastScroll ->
                    if lastScroll == oldTime then
                        -- hasn't changed in a second
                        -- Then write history!and clear it!
                        ( { model
                            | wheelState =
                                Dict.remove
                                    id
                                    model.wheelState
                            , history =
                                appendHistory
                                    { action = action
                                    , objectsState = model.objects
                                    }
                                    model.history
                          }
                        , saveToUrl model.navigationKey model.objects
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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

        SetTransformation label lens transformation ->
            let
                newObjects =
                    updateObject
                        model.selectedObjectId
                        (lens.set transformation)
                        model.objects
            in
            ( { model
                | objects = newObjects
                , history =
                    appendHistory
                        { action = ChangeTransformationType transformation label
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
                                | selectedObjectId = ObjectDict.anyId newObjects
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
                    ObjectDict.insert
                        (Object.initWithShape canvasWidth canvasHeight newShape)
                        model.objects
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



-- Update Helpers


updateObject : Maybe ObjectDict.Id -> (Object -> Object) -> ObjectDict -> ObjectDict
updateObject maybeId updater objects =
    Maybe.map (\id -> ObjectDict.updateObject id updater objects) maybeId
        |> Maybe.withDefault objects


saveToUrl : Key -> ObjectDict -> Cmd Msg
saveToUrl key objects =
    objects
        |> ObjectDict.encode
        |> Json.Encode.encode 0
        |> Base64.encode
        |> String.append "#"
        |> Browser.Navigation.replaceUrl key


appendHistory : Snapshot -> SelectList Snapshot -> SelectList Snapshot
appendHistory newSnapshot history =
    history
        |> SelectList.updateBefore (always [])
        |> SelectList.insertAfter newSnapshot


download : String -> String -> Cmd msg
download fileName svg =
    Download.string (String.append fileName ".svg") "image/svg+xml" svg


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
                            , id = "shape-x"
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Y"
                            , lens = Object.yLens
                            , id = "shape-y"
                            }
                    ]
                , withSelectedObject model emptyHtml <|
                    sizeAttributes
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Rotate"
                            , lens = Object.baseRotation
                            , id = "shape-rotate"
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
                            , id = "shape-loops"
                            }
                    ]
                , controlSubSection "Scale"
                , withSelectedObject model emptyHtml <|
                    transformationView "Scale" Object.scaleLens
                , controlSubSection "X-Shift"
                , withSelectedObject model emptyHtml <|
                    transformationView "X Shift" Object.xShiftLens
                , controlSubSection "Y-Shift"
                , withSelectedObject model emptyHtml <|
                    transformationView "Y Shift" Object.yShiftLens
                , controlSubSection "Rotation"
                , controlRow <|
                    [ withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Anchor X"
                            , lens = Object.anchorXLens
                            , id = "shape-anchor-x"
                            }
                    , withSelectedObject model emptyHtml <|
                        numberInput
                            { label = "Anchor Y"
                            , lens = Object.anchorYLens
                            , id = "shape-anchor-y"
                            }
                    ]
                , withSelectedObject model emptyHtml <|
                    transformationView "Rotation" Object.rotationLens
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
    , id : String
    }


sizeAttributes : Object -> Html Msg
sizeAttributes object =
    case object.shape of
        Circle cd ->
            controlRow <|
                [ numberInput
                    { label = "Radius"
                    , lens = Lens.compose Object.shapeLens (Shape.radiusLens 10)
                    , id = "size-radius"
                    }
                    object
                ]

        Square sd ->
            controlRow <|
                [ numberInput
                    { label = "Width"
                    , lens = Lens.compose Object.shapeLens (Shape.widthLens 10)
                    , id = "size-width"
                    }
                    object
                , numberInput
                    { label = "Height"
                    , lens = Lens.compose Object.shapeLens (Shape.heightLens 10)
                    , id = "size-height"
                    }
                    object
                ]


numberInput : NumberInputConfig -> Object -> Html Msg
numberInput { label, lens, id } object =
    let
        action =
            ChangeField label

        inputMsg str =
            Maybe.withDefault (.get lens object) (String.toFloat str)
                |> SetWithLens action lens

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
        , Wheel.onWheel <| AttributeWheel action id lens
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


transformationView : String -> Lens Object Transformation -> Object -> Html Msg
transformationView label lens object =
    let
        transformationMsg =
            SetTransformation label lens

        transformation =
            lens.get object

        transformationEq t =
            t == Transformation.label transformation

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

        makeId l =
            String.concat
                [ String.toLower label
                , "-"
                , l
                ]

        renderToggler =
            case transformation of
                Linear float ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Number"
                            , lens = Lens.compose lens (Transformation.linearLens 0)
                            , id = makeId "number"
                            }
                            object
                        ]
                    ]

                Cyclical d ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Amplitude"
                            , lens = Lens.compose lens (Transformation.amplitudeLens 0)
                            , id = makeId "amplitude"
                            }
                            object
                        , numberInput
                            { label = "Frequency"
                            , lens = Lens.compose lens (Transformation.frequencyLens 0)
                            , id = makeId "frequency"
                            }
                            object
                        ]
                    ]

                Random d ->
                    [ controlRow <|
                        [ numberInput
                            { label = "Min"
                            , lens = Lens.compose lens (Transformation.minLens 0)
                            , id = makeId "min"
                            }
                            object
                        , numberInput
                            { label = "Max"
                            , lens = Lens.compose lens (Transformation.maxLens 0)
                            , id = makeId "max"
                            }
                            object
                        ]
                    , controlRow <|
                        [ numberInput
                            { label = "Rand"
                            , lens = Lens.compose lens (Transformation.randLens 0)
                            , id = makeId "rand"
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
            { text = Object.label obj ++ " " ++ String.fromInt id
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


isSelected : Model -> ObjectDict.Id -> Bool
isSelected model id =
    Just id == model.selectedObjectId


getSelectedObject : Model -> Maybe Object
getSelectedObject model =
    Maybe.map (\id -> Dict.get id model.objects) model.selectedObjectId
        |> Maybe.join


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
            (squareData.width + transformationByLoop loop object.scale)
                |> max 0

        Circle circleData ->
            (2 * circleData.radius + transformationByLoop loop object.scale)
                |> max 0


calculatedHeight : Int -> Object -> Float
calculatedHeight loop object =
    case object.shape of
        Square squareData ->
            (squareData.height + transformationByLoop loop object.scale)
                |> max 0

        Circle circleData ->
            (2 * circleData.radius + transformationByLoop loop object.scale)
                |> max 0


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



-- ports


port gotSvg : (String -> msg) -> Sub msg


port getSvg : String -> Cmd msg


port capture : Int -> Cmd msg
