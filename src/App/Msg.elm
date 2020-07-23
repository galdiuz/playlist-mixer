module App.Msg exposing (Msg(..))

import Http
import Json.Encode as Encode
import Time

import App
import Youtube.Page exposing (Page)
import Youtube.Playlist exposing (Playlist)
import Youtube.Video exposing (Video)


type Msg
    = NoOp
    | YouTubeApiReady
    | PlayerReady
    | PlayerStateChange Encode.Value
    | PlayerError Encode.Value
    | SignIn
    | ReceiveFromStorage App.StorageValue
    | StorageChanged App.StorageValue
    | StorageDeleted String
    | SetTime Time.Posix
    | ReceiveRandomBytes (List Int)
    | GetUserPlaylists
    | GetUserPlaylistsResult Int (List Playlist) (Result Http.Error (Page Playlist))
    | GetPlaylistVideos (List Playlist)
    | GetPlaylistVideosResult
        (List App.VideoListItem)
        (List Playlist)
        Playlist
        Int
        (Result Http.Error (Page Video))
    | SetListChecked String Bool
    | SetListAll
    | SetListNone
    | LoadListFromStorage
    | ToggleEditVideo Int Bool
    | SetVideoStartAt Int String
    | SetVideoEndAt Int String
    | SetVideoNote Int String
    | SaveVideoTimes Int
    | SaveVideoTimesResult Int (Result Http.Error Video)
    | ValidateVideoStartAt Int
    | ValidateVideoEndAt Int
    | PlayVideo Int
    | SetPlaylist (List App.VideoListItem)
    | ScrollToCurrentVideo
    | PlayNext
    | PlayPrevious
