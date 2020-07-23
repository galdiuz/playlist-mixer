module Youtube.Playlist exposing
    ( Playlist
    , decoder
    , encode
    , url
    )

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode


type alias Playlist =
    { id : String
    , title : String
    }


decoder : Decode.Decoder Playlist
decoder =
    Field.require "id" Decode.string <| \id ->
    Field.require "title" Decode.string <| \title ->
    Decode.succeed
        { id = id
        , title = title
        }


encode : Playlist -> Encode.Value
encode playlist =
    Encode.object
        [ ( "id", Encode.string playlist.id )
        , ( "title", Encode.string playlist.title )
        ]


url : Playlist -> String
url playlist =
    "https://www.youtube.com/playlist?list=" ++ playlist.id
