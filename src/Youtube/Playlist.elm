module Youtube.Playlist exposing
    ( Playlist
    , decoder
    )

import Json.Decode as Decode
import Json.Decode.Field as Field


type alias Playlist =
    { id : String
    , title : String
    }


decoder : Decode.Decoder Playlist
decoder =
    Field.require "id" Decode.string <| \id ->
    Field.requireAt [ "snippet", "title" ] Decode.string <| \title ->
    Decode.succeed
        { id = id
        , title = title
        }
