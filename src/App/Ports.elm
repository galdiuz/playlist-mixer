port module App.Ports exposing (..)

import Json.Encode as Encode

import App

port onYouTubeApiReady : (Encode.Value -> msg) -> Sub msg
port onPlayerStateChange : (Encode.Value -> msg) -> Sub msg
port onPlayerReady : (Encode.Value -> msg) -> Sub msg
port onPlayerError : (Encode.Value -> msg) -> Sub msg
port createPlayer : String -> Cmd msg
port playVideo : App.PlayVideoData -> Cmd msg

port saveToStorage : App.StorageValue -> Cmd msg
port loadFromStorage : String -> Cmd msg
port removeFromStorage : String -> Cmd msg
port receiveFromStorage : (App.StorageValue -> msg) -> Sub msg

port generateRandomBytes : Int -> Cmd msg
port receiveRandomBytes : (List Int -> msg) -> Sub msg

port consoleErr : String -> Cmd msg
