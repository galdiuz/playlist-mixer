module App exposing
    (..)
    -- ( Flags
    -- , Msg(..)
    -- , State
    -- , StorageValue
    -- , Token
    -- )

import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element
import OAuth
import OAuth.Implicit as OAuth
import Json.Encode as Encode
import String.Format
import Url exposing (Url)
import Url.Builder
import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import Json.Encode.Extra as Encode

import Google.OAuth.Scope exposing (Scope)
import Youtube.Playlist as Playlist exposing (Playlist)
import Youtube.Video as Video exposing (Video)


type alias Flags =
    { bytes : List Int
    , oauthClientId : String
    , playlistInStorage : Bool
    , playlistStorageKey : String
    , time : Int
    , token : Encode.Value
    , tokenStorageKey : String
    }


type alias State =
    { lists : Dict String PlaylistListItem
    , messages : List String
    , navigationKey : Navigation.Key
    , redirectUri : Url
    , time : Int
    , token : Maybe Token
    , videos : Dict Int VideoListItem
    , current : Int
    , playlistInStorage : Bool
    , playlistStorageKey : String
    , tokenStorageKey : String
    , oauthClientId : String
    , youtubeApiReady : Bool
    , theme : Theme
    , oauthResult : OAuth.AuthorizationResult
    , playlistsByUrl : String
    , playlistsByChannel : String
    , oauthScopes : List Scope
    , autoplay : Bool
    }


type alias Theme =
    { bg : Element.Color
    , fg : Element.Color
    , disabled : Element.Color
    , error : Element.Color
    }


type alias StorageValue =
    { key : String
    , value : Encode.Value
    }


type alias Token =
    { email : Maybe String
    , expires : Int
    , scopes : List Scope
    , token : OAuth.Token
    }


type alias PlayVideoData =
    { videoId : String
    , startSeconds : Maybe Int
    , endSeconds : Maybe Int
    }


type alias PlaylistListItem =
    { checked : Bool
    , playlist : Playlist
    }


type alias VideoListItem =
    { video : Video
    , playlist : Playlist
    , startAt : String
    , startAtError : Maybe String
    , endAt : String
    , endAtError : Maybe String
    , note : String
    , editOpen : Bool
    }


lightTheme : Theme
lightTheme =
    { bg = Element.rgb 0.9 0.9 0.9
    , fg = Element.rgb 0.1 0.1 0.1
    , disabled = Element.rgb 0.5 0.5 0.5
    , error = Element.rgb 1 0 0
    }


darkTheme : Theme
darkTheme =
    { bg = Element.rgb 0.1 0.1 0.1
    , fg = Element.rgb 0.7 0.7 0.7
    , disabled = Element.rgb 0.4 0.4 0.4
    , error = Element.rgb 1 0 0
    }


makeVideoListItem : Playlist -> Video -> VideoListItem
makeVideoListItem playlist video =
    { video = video
    , playlist = playlist
    , startAt = ""
    , startAtError = Nothing
    , endAt = ""
    , endAtError = Nothing
    , note = ""
    , editOpen = False
    }


encodeVideoListItem : VideoListItem -> Encode.Value
encodeVideoListItem item =
    Encode.object
        [ ( "video", Video.encode item.video )
        , ( "playlist", Playlist.encode item.playlist )
        ]


videoListItemDecoder : Decode.Decoder VideoListItem
videoListItemDecoder =
    Field.require "video" Video.decoder <| \video ->
    Field.require "playlist" Playlist.decoder <| \playlist ->
    Decode.succeed (makeVideoListItem playlist video)


encodePlaylist : Int -> List VideoListItem -> Encode.Value
encodePlaylist current items =
    Encode.object
        [ ( "current", Encode.int current )
        , ( "items", Encode.list encodeVideoListItem items )
        ]


decodePlaylist : Decode.Decoder { current : Int, items : List VideoListItem }
decodePlaylist =
    Field.require "current" Decode.int <| \current ->
    Field.require "items" (Decode.list videoListItemDecoder) <| \items ->
    Decode.succeed
        { current = current
        , items = items
        }


encodeToken : Token -> Encode.Value
encodeToken token =
    Encode.object
        [ ( "email", Encode.maybe Encode.string token.email )
        , ( "expires", Encode.int token.expires )
        , ( "scopes", Encode.list Encode.string <| List.map Google.OAuth.Scope.toString token.scopes )
        , ( "token", Encode.string <| OAuth.tokenToString token.token )
        ]


decodeToken : Decode.Decoder Token
decodeToken =
    Field.require "email" (Decode.maybe Decode.string) <| \email ->
    Field.require "expires" Decode.int <| \expires ->
    Field.require "scopes" (Decode.list Google.OAuth.Scope.decoder) <| \scopes ->
    Field.require "token" Decode.string <| \token ->
    case OAuth.tokenFromString token of
        Just t ->
            Decode.succeed
                { email = email
                , expires = expires
                , scopes = scopes
                , token = t
                }

        _ ->
            Decode.fail "Unable to parse token."


nextIndex : State -> Int
nextIndex state =
    if Dict.size state.videos > state.current + 1 then
        state.current + 1
    else
        0


previousIndex : State -> Int
previousIndex state =
    if state.current == 0 then
        Dict.size state.videos - 1
    else
        max 0 (state.current - 1)


secondsToString : Maybe Int -> String
secondsToString maybe =
    case maybe of
        Just seconds ->
            "{{}}:{{}}"
                |> String.Format.value (String.fromInt <| seconds // 60)
                |> String.Format.value (String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds)

        Nothing ->
            ""


privacyPolicyUrl : State -> String
privacyPolicyUrl state =
    Url.Builder.relative
        [ "privacy-policy.md" ]
        []
