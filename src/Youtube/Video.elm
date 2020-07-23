module Youtube.Video exposing
    ( Video
    , decoder
    , encode
    , url
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
    , position : Int
    , playlistId : String
    , itemId : String
    , note : Maybe String
    }


decoder : Decode.Decoder Video
decoder =
    Field.require "id" Decode.string <| \id ->
    Field.require "title" Decode.string <| \title ->
    Field.require "startAt" (Decode.nullable Decode.int) <| \startAt ->
    Field.require "endAt" (Decode.nullable Decode.int) <| \endAt ->
    Field.require "position" Decode.int <| \position ->
    Field.require "playlistId" Decode.string <| \playlistId ->
    Field.require "itemId" Decode.string <| \itemId ->
    Field.require "note" (Decode.nullable Decode.string) <| \note ->
    Decode.succeed
        { id = id
        , title = title
        , startAt = startAt
        , endAt = endAt
        , position = position
        , playlistId = playlistId
        , itemId = itemId
        , note = note
        }


encode : Video -> Encode.Value
encode video =
    Encode.object
        [ ( "id", Encode.string video.id )
        , ( "title", Encode.string video.title )
        , ( "startAt", Encode.maybe Encode.int video.startAt )
        , ( "endAt", Encode.maybe Encode.int video.endAt )
        , ( "position", Encode.int video.position )
        , ( "playlistId", Encode.string video.playlistId )
        , ( "itemId", Encode.string video.itemId )
        , ( "note", Encode.maybe Encode.string video.note )
        ]


url : Video -> String
url video =
    "https://www.youtube.com/watch?v=" ++ video.id
