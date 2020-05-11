module ObjectDict exposing (..)

import Base64
import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Object exposing (Object)
import Result.Extra


type alias Id =
    Int


type alias ObjectDict =
    Dict Id Object


fromBase64 : String -> Result String ObjectDict
fromBase64 base64 =
    base64
        |> Base64.decode
        |> Result.map (Decode.decodeString decoder)
        |> Result.map (Result.mapError (always "Couldn't decode from URL"))
        |> Result.Extra.join


insert : Object -> ObjectDict -> ( Id, ObjectDict )
insert obj objectDict =
    let
        newKey =
            nextKey objectDict
    in
    ( newKey
    , Dict.insert newKey obj objectDict
    )


updateObject : Id -> (Object -> Object) -> ObjectDict -> ObjectDict
updateObject id updater objectDict =
    Dict.update id
        (Maybe.map updater)
        objectDict


anyId : ObjectDict -> Maybe Id
anyId objectDict =
    objectDict
        |> Dict.keys
        |> List.head


nextKey : ObjectDict -> Int
nextKey dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


decoder : Decoder ObjectDict
decoder =
    Decode.dict Object.decoder
        |> Decode.map (Dict.Extra.mapKeys (Maybe.withDefault 0 << String.toInt))


encode : ObjectDict -> Encode.Value
encode objectDict =
    Encode.dict String.fromInt Object.encode objectDict
