module App.Json exposing (encodeToken, decodeToken)

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import OAuth

import App


encodeToken : App.Token -> Encode.Value
encodeToken token =
    Encode.object
        [ ( "expires", Encode.int token.expires )
        , ( "scopes", Encode.list Encode.string token.scopes )
        , ( "token", Encode.string <| OAuth.tokenToString token.token )
        ]


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
