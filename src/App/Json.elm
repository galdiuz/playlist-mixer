module App.Json exposing (encodeToken, decodeToken)

import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import OAuth

import App


encodeToken : App.Token -> Encode.Value
encodeToken token =
    Encode.object
        [ ( "token", Encode.string <| OAuth.tokenToString token.token )
        , ( "expires", Encode.int token.expires )
        ]


decodeToken : Decode.Decoder App.Token
decodeToken =
    Field.require "token" Decode.string <| \token ->
    Field.require "expires" Decode.int <| \expires ->
    case OAuth.tokenFromString token of
        Just t ->
            Decode.succeed
                { token = t
                , expires = expires
                }
        _ ->
            Decode.fail "Unable to parse token"
