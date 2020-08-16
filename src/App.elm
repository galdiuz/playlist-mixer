module App exposing
    (..)
    -- ( Flags
    -- , Msg(..)
    -- , State
    -- , StorageValue
    -- , Token
    -- )

import Array exposing (Array)
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
import Youtube.PlayerError exposing (PlayerError)
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
    { autoplay : Bool
    , currentListIndex : Int
    , currentVideoIndex : Int
    , messages : List String
    , oauthClientId : String
    , oauthResult : OAuth.AuthorizationResult
    , oauthScopes : List Scope
    , playlistInStorage : Bool
    , playlistList : Dict String PlaylistListItem
    , playlistStorageKey : String
    , playlistsByChannelValue : String
    , playlistsByUrlValue : String
    , redirectUri : Url
    , scrolling : Bool
    , searchValue : String
    , theme : Theme
    , time : Int
    , token : Maybe Token
    , tokenStorageKey : String
    , videos : Dict String Video
    , videoList : Array VideoListItem
    , youtubeApiReady : Bool
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
    { editOpen : Bool
    , editSegments : Array VideoListItemSegment
    , error : Maybe PlayerError
    , playlist : Playlist
    , segmentIndex : Int
    , videoId : String
    }


type alias VideoListItemSegment =
    { endAtError : Maybe String
    , endAtValue : String
    , note : String
    , startAtError : Maybe String
    , startAtValue : String
    }


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Int
    , containerHeight : Int
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


makeVideoListItems : Playlist -> Video -> List VideoListItem
makeVideoListItems playlist video =
    case video.segments of
        [] ->
            [ makeVideoListItem playlist video 0 ]

        segments ->
            List.range 0 (List.length segments - 1)
                |> List.map (makeVideoListItem playlist video)


makeVideoListItem : Playlist -> Video -> Int -> VideoListItem
makeVideoListItem playlist video index =
    { editOpen = False
    , editSegments = Array.empty
    , error = Nothing
    , playlist = playlist
    , segmentIndex = index
    , videoId = video.id
    }


encodeVideoListItem : VideoListItem -> Encode.Value
encodeVideoListItem item =
    Encode.object
        []
        -- [ ( "video", Video.encode item.video )
        -- , ( "playlist", Playlist.encode item.playlist )
        -- ]


-- videoListItemDecoder : Decode.Decoder VideoListItem
-- videoListItemDecoder =
--     Field.require "video" Video.decoder <| \video ->
--     Field.require "playlist" Playlist.decoder <| \playlist ->
--     Decode.succeed (makeVideoListItem playlist video)


encodePlaylist : Int -> List VideoListItem -> Encode.Value
encodePlaylist current items =
    Encode.object
        [ ( "current", Encode.int current )
        , ( "items", Encode.list encodeVideoListItem items )
        ]


decodePlaylist : Decode.Decoder { current : Int, items : List VideoListItem }
decodePlaylist =
    Field.require "current" Decode.int <| \current ->
    -- Field.require "items" (Decode.list videoListItemDecoder) <| \items ->
    Decode.succeed
        { current = current
        -- , items = items
        , items = []
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
    if Array.length state.videoList > state.currentVideoIndex + 1 then
        state.currentVideoIndex + 1
    else
        0


previousIndex : State -> Int
previousIndex state =
    if state.currentVideoIndex == 0 then
        Array.length state.videoList - 1
    else
        max 0 (state.currentVideoIndex - 1)


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


videoListPageSize : Int
videoListPageSize =
    50


videoListScrollSize : Int
videoListScrollSize =
    20


decodeScrollPos : Decode.Decoder ScrollPos
decodeScrollPos =
    Decode.map3
        (\scrollTop content container ->
            { scrollTop = scrollTop
            , contentHeight = content
            , containerHeight = container
            }
        )
        (Decode.oneOf
            [ Decode.at [ "target", "scrollTop" ] Decode.float
            , Decode.at [ "target", "scrollingElement", "scrollTop" ] Decode.float
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "target", "scrollHeight" ] Decode.int
            , Decode.at [ "target", "scrollingElement", "scrollHeight" ] Decode.int
            ]
        )
        (Decode.map2 Basics.max offsetHeight clientHeight)


offsetHeight : Decode.Decoder Int
offsetHeight =
    Decode.oneOf
        [ Decode.at [ "target", "offsetHeight" ] Decode.int
        , Decode.at [ "target", "scrollingElement", "offsetHeight" ] Decode.int
        ]


clientHeight : Decode.Decoder Int
clientHeight =
    Decode.oneOf
        [ Decode.at [ "target", "clientHeight" ] Decode.int
        , Decode.at [ "target", "scrollingElement", "clientHeight" ] Decode.int
        ]


getVideoItem : State -> Int -> Maybe { video : Video, listItem : VideoListItem }
getVideoItem state index =
    Array.get index state.videoList
        |> Maybe.andThen
            (\listItem ->
                Dict.get listItem.videoId state.videos
                    |> Maybe.map
                        (\video ->
                            { video = video
                            , listItem = listItem
                            }
                        )
            )
