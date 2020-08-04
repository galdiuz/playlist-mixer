module Google.OAuth.Scope exposing
    ( Scope(..)
    , decoder
    , fromString
    , toString
    )

import Json.Decode as Decode


type Scope
    = Email
    | OpenId
    | Youtube
    | YoutubeReadOnly


decoder : Decode.Decoder Scope
decoder =
    Decode.andThen
        (\string ->
            case fromString string of
                Just scope ->
                    Decode.succeed scope

                Nothing ->
                    Decode.fail <| "Not a valid scope: '" ++ string ++ "'."
        )
        Decode.string


fromString : String -> Maybe Scope
fromString string =
    case string of
        "email" ->
            Just Email

        "openid" ->
            Just OpenId

        "https://www.googleapis.com/auth/youtube" ->
            Just Youtube

        "https://www.googleapis.com/auth/youtube.readonly" ->
            Just YoutubeReadOnly

        _ ->
            Nothing


toString : Scope -> String
toString scope =
    case scope of
        Email ->
            "email"

        OpenId ->
            "openid"

        Youtube ->
            "https://www.googleapis.com/auth/youtube"

        YoutubeReadOnly ->
            "https://www.googleapis.com/auth/youtube.readonly"
