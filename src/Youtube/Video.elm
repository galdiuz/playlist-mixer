module Youtube.Video exposing
    ( Video
    , decoder
    , encode
    )

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import Json.Encode.Extra as Encode


type alias Video =
    { id : String
    , title : String
    , startAt : Maybe Int
    , endAt : Maybe Int
    }


decoder : Decode.Decoder Video
decoder =
    Field.require "id" Decode.string <| \id ->
    Field.require "title" Decode.string <| \title ->
    Field.require "startAt" (Decode.nullable Decode.int) <| \startAt ->
    Field.require "endAt" (Decode.nullable Decode.int) <| \endAt ->
    Decode.succeed
        { id = id
        , title = title
        , startAt = startAt
        , endAt = endAt
        }


encode : Video -> Encode.Value
encode video =
    Encode.object
        [ ( "id", Encode.string video.id )
        , ( "title", Encode.string video.title )
        , ( "startAt", Encode.maybe Encode.int video.startAt )
        , ( "endAt", Encode.maybe Encode.int video.endAt )
        ]
