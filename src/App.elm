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
    { time : Int
    , bytes : List Int
    , token : Encode.Value
    , storedList : StorageValue
    }


type alias State =
    { lists : Dict String (ListItem Playlist)
    , messages : List String
    , navigationKey : Navigation.Key
    , redirectUri : Url
    , time : Int
    , token : Maybe Token
    , videos : Dict Int Video
    , current : Int
    }


type alias StorageValue =
    { key : String
    , value : Encode.Value
    }


type alias Token =
    { token : OAuth.Token
    , expires : Int
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
