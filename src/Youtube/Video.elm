module Youtube.Video exposing
    ( Video
    , Segment
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
    , itemId : String
    , note : Maybe String
    , playlistId : String
    , position : Int
    , segments : List Segment
    , title : String
    }


type alias Segment =
    { endAt : Maybe Int
    , startAt : Maybe Int
    , title : Maybe String
    }


decoder : Decode.Decoder Video
decoder =
    Field.require "id" Decode.string <| \id ->
    Field.require "itemId" Decode.string <| \itemId ->
    Field.require "note" (Decode.nullable Decode.string) <| \note ->
    Field.require "playlistId" Decode.string <| \playlistId ->
    Field.require "position" Decode.int <| \position ->
    Field.require "segments" (Decode.list segmentDecoder) <| \segments ->
    Field.require "title" Decode.string <| \title ->
    Decode.succeed
        { id = id
        , itemId = itemId
        , note = note
        , playlistId = playlistId
        , position = position
        , segments = segments
        , title = title
        }


segmentDecoder : Decode.Decoder Segment
segmentDecoder =
    Field.require "endAt" (Decode.nullable Decode.int) <| \endAt ->
    Field.require "startAt" (Decode.nullable Decode.int) <| \startAt ->
    Field.require "title" (Decode.nullable Decode.string) <| \title ->
    Decode.succeed
        { endAt = endAt
        , startAt = startAt
        , title = title
        }


encode : Video -> Encode.Value
encode video =
    Encode.object
        [ ( "id", Encode.string video.id )
        , ( "itemId", Encode.string video.itemId )
        , ( "note", Encode.maybe Encode.string video.note )
        , ( "playlistId", Encode.string video.playlistId )
        , ( "position", Encode.int video.position )
        , ( "segments", Encode.list encodeSegment video.segments )
        , ( "title", Encode.string video.title )
        ]


encodeSegment : Segment -> Encode.Value
encodeSegment segment =
    Encode.object
        [ ( "endAt", Encode.maybe Encode.int segment.endAt )
        , ( "startAt", Encode.maybe Encode.int segment.startAt )
        , ( "title", Encode.maybe Encode.string segment.title )
        ]


url : Video -> String
url video =
    "https://www.youtube.com/watch?v=" ++ video.id
