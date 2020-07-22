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
    | GetPlaylistVideos (Result String (List Video) -> Msg) Playlist
    | GetPlaylistVideosResult
        (Result String (List Video) -> Msg)
        Playlist
        Int
        (List Video)
        (Result Http.Error (Page Video))
    | GetAllVideos (List Playlist)
    | GetAllVideosResult
        (List Video)
        (List Playlist)
        Playlist
        (Result String (List Video))
    | SetListChecked String Bool
    | SetListAll
    | SetListNone
    | LoadListFromStorage
    | ToggleEditVideo Int Bool
    | SetVideoStartAt Int String
    | SetVideoEndAt Int String
    | SaveVideoTimes Int
    | ValidateVideoStartAt Int
    | ValidateVideoEndAt Int
