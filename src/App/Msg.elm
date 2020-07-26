module App.Msg exposing
    ( Msg(..)
    , OAuthMsg(..)
    , PlayerMsg(..)
    , PlaylistListMsg(..)
    , StorageMsg(..)
    , VideoListMsg(..)
    )

import Http
import Json.Encode as Encode
import Time

import App
import Youtube.Page exposing (Page)
import Youtube.Playlist exposing (Playlist)
import Youtube.Video exposing (Video)


type Msg
    = NoOp
    | SetTime Time.Posix
    | OAuth OAuthMsg
    | Player PlayerMsg
    | PlaylistList PlaylistListMsg
    | Storage StorageMsg
    | VideoList VideoListMsg


type OAuthMsg
    = ReceiveRandomBytes (List Int)
    | SignIn


type PlayerMsg
    = PlayNext
    | PlayPrevious
    | PlayerError Encode.Value
    | PlayerReady
    | PlayerStateChange Encode.Value
    | YouTubeApiReady


type PlaylistListMsg
    = GetPlaylistVideos (List Playlist)
    | GetPlaylistVideosResult
        (List App.VideoListItem)
        (List Playlist)
        Playlist
        Int
        (Result Http.Error (Page Video))
    | GetPlaylistsResult Int (List Playlist) (Result Http.Error (Page Playlist))
    | GetUserPlaylists
    | LoadListFromStorage
    | LoadPlaylistsByUrl
    | SetChecked String Bool
    | SetCheckedAll
    | SetCheckedNone
    | SetPlaylist (List App.VideoListItem)
    | SetPlaylistsByUrl String


type StorageMsg
    = ReceiveFromStorage App.StorageValue
    | StorageChanged App.StorageValue
    | StorageDeleted String


type VideoListMsg
    = PlayVideo Int
    | SaveVideoTimes Int
    | SaveVideoTimesResult Int (Result Http.Error Video)
    | ScrollToCurrentVideo
    | SetVideoEndAt Int String
    | SetVideoNote Int String
    | SetVideoStartAt Int String
    | ToggleEditVideo Int Bool
    | ValidateVideoEndAt Int
    | ValidateVideoStartAt Int
