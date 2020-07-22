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
import OAuth
import Json.Encode as Encode
import Url exposing (Url)

import Youtube.Playlist exposing (Playlist)
import Youtube.Video exposing (Video)


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
    { lists : Dict String (ListItem Playlist)
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
    }


type alias StorageValue =
    { key : String
    , value : Encode.Value
    }


type alias Token =
    { expires : Int
    , scopes : List String
    , token : OAuth.Token
    }


type alias PlayVideoData =
    { videoId : String
    , startSeconds : Int
    , endSeconds : Int
    }


type alias ListItem a =
    { checked : Bool
    , item : a
    }


type alias VideoListItem =
    { video: Video
    , startAt : String
    , startAtError : Maybe String
    , endAt : String
    , endAtError : Maybe String
    , editOpen : Bool
    }


videoToListItem : Video -> VideoListItem
videoToListItem video =
    { video = video
    , startAt = ""
    , startAtError = Nothing
    , endAt = ""
    , endAtError = Nothing
    , editOpen = False
    }
