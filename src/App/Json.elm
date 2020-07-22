module App.Json exposing
    ( encodePlaylist
    , encodeToken
    , decodePlaylist
    , decodeToken
    )

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import OAuth

import App
import Youtube.Video as Video exposing (Video)


encodePlaylist : Int -> List Video -> Encode.Value
encodePlaylist current videos =
    Encode.object
        [ ( "current", Encode.int current )
        , ( "videos", Encode.list Video.encode videos )
        ]

encodeToken : App.Token -> Encode.Value
encodeToken token =
    Encode.object
        [ ( "expires", Encode.int token.expires )
        , ( "scopes", Encode.list Encode.string token.scopes )
        , ( "token", Encode.string <| OAuth.tokenToString token.token )
        ]


decodePlaylist : Decode.Decoder { current : Int, videos : List Video }
decodePlaylist =
    Field.require "current" Decode.int <| \current ->
    Field.require "videos" (Decode.list Video.decoder) <| \videos ->
    Decode.succeed
        { current = current
        , videos = videos
        }


decodeToken : Decode.Decoder App.Token
decodeToken =
    Field.require "token" Decode.string <| \token ->
    Field.require "scopes" (Decode.list Decode.string) <| \scopes ->
    Field.require "expires" Decode.int <| \expires ->
    case OAuth.tokenFromString token of
        Just t ->
            Decode.succeed
                { expires = expires
                , scopes = scopes
                , token = t
                }
        _ ->
            Decode.fail "Unable to parse token."
