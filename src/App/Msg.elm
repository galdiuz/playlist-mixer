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
import Google.OAuth.Scope exposing (Scope)
import Youtube.Page exposing (Page)
import Youtube.Playlist exposing (Playlist)
import Youtube.Video exposing (Video)


type Msg
    = NoOp
    | SetAutoplay Bool
    | SetTime Time.Posix
    | SetTheme App.Theme
    | OAuth OAuthMsg
    | Player PlayerMsg
    | PlaylistList PlaylistListMsg
    | Storage StorageMsg
    | VideoList VideoListMsg


type OAuthMsg
    = GetUserEmailResult (Result Http.Error String)
    | ReceiveRandomBytes (List Int)
    | SignIn (List Scope)
    | SignOut


type PlayerMsg
    = PlayNext
    | PlayPrevious
    | PlayerError Encode.Value
    | PlayerReady
    | PlayerStateChange Encode.Value
    | YouTubeApiReady


type PlaylistListMsg
    = AppendPlaylist (List App.VideoListItem)
    | GetPlaylistVideos (List App.VideoListItem -> Msg)
    | GetPlaylistVideosResult
        (List App.VideoListItem -> Msg)
        (List App.VideoListItem)
        (List Playlist)
        Playlist
        Int
        (Result Http.Error (Page Video))
    | GetPlaylistsResult Int (List Playlist) (Result Http.Error (Page Playlist))
    | GetUserPlaylists
    | LoadListFromStorage
    | LoadPlaylistsByUrl
    | LoadPlaylistsByChannel
    | SetChecked String Bool
    | SetCheckedAll
    | SetCheckedNone
    | SetPlaylist (List App.VideoListItem)
    | SetPlaylistsByChannel String
    | SetPlaylistsByUrl String


type StorageMsg
    = ReceiveFromStorage App.StorageValue
    | StorageChanged App.StorageValue
    | StorageDeleted String


type VideoListMsg
    = PlayVideo Int
    | SaveVideoTimes Int
    | SaveVideoTimesResult Int (Result Http.Error Video)
    | Scroll App.ScrollPos
    | ScrollEarlier Float
    | ScrollLater Float
    | ScrollToCurrentVideo
    | SetScrolling
    | SetSearch String
    | SetVideoEndAt Int String
    | SetVideoNote Int String
    | SetVideoStartAt Int String
    | ShowCurrentVideo
    | ShowEarlierVideos
    | ShowLaterVideos
    | ToggleEditVideo Int Bool
    | ValidateVideoEndAt Int
    | ValidateVideoStartAt Int
