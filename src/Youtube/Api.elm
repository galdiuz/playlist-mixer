module Youtube.Api exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Field as Field
import OAuth
import Url.Builder
import Maybe.Extra

import App
import Youtube.Page exposing (Page)
import Youtube.Playlist exposing (Playlist)
import Youtube.Video exposing (Video)


getUserPlaylists :
    Maybe App.Token
    -> Maybe String
    -> (Result Http.Error (Page Playlist) -> msg)
    -> Cmd msg
getUserPlaylists oauthToken pageToken toMsg =
    get
        { url = userPlaylistsUrl pageToken
        , expect = Http.expectJson toMsg (Youtube.Page.decoder Youtube.Playlist.decoder)
        , token = oauthToken
        }


getPlaylistVideos :
    String
    -> Maybe App.Token
    -> Maybe String
    -> (Result Http.Error (Page Video) -> msg)
    -> Cmd msg
getPlaylistVideos playlistId oauthToken pageToken toMsg =
    get
        { url = playlistVideosUrl playlistId pageToken
        , expect = Http.expectJson toMsg (Youtube.Page.decoder videoDecoder)
        , token = oauthToken
        }


playlistVideosUrl : String -> Maybe String -> String
playlistVideosUrl playlistId pageToken =
    Url.Builder.crossOrigin
        "https://www.googleapis.com/youtube/v3/playlistItems"
        []
        [ Url.Builder.string "part" "snippet,contentDetails"
        , Url.Builder.string "playlistId" playlistId
        , Url.Builder.int "maxResults" 50
        , Url.Builder.string "pageToken" <| Maybe.withDefault "" pageToken
        ]


userPlaylistsUrl : Maybe String -> String
userPlaylistsUrl pageToken =
    Url.Builder.crossOrigin
        "https://www.googleapis.com/youtube/v3/playlists"
        []
        [ Url.Builder.string "part" "snippet"
        , Url.Builder.int "maxResults" 50
        , Url.Builder.string "mine" "true"
        , Url.Builder.string "pageToken" <| Maybe.withDefault "" pageToken
        ]


type alias GetParams msg =
    { url : String
    , expect : Http.Expect msg
    , token : Maybe App.Token
    }


get : GetParams msg -> Cmd msg
get { url, expect, token } =
    Http.request
        { method = "GET"
        , headers =
            case token of
                Just t ->
                    OAuth.useToken t.token []
                Nothing ->
                    []
        , url = url
        , expect = expect
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


videoDecoder : Decode.Decoder Video
videoDecoder =
    Field.requireAt [ "contentDetails", "videoId" ] Decode.string <| \id ->
    Field.requireAt [ "snippet", "title" ] Decode.string <| \title ->
    Field.attemptAt [ "contentDetails", "startAt" ] Decode.int <| \startAt ->
    Field.attemptAt [ "contentDetails", "endAt" ] Decode.int <| \endAt ->
    Decode.succeed
        { id = id
        , title = title
        , startAt = startAt
        , endAt = endAt
        }
