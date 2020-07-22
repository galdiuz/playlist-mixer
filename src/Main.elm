module Main exposing (main)

import Base64.Encode as Base64
import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Bytes.Encode as Bytes
import Cmd.Extra
import Dict
import Http
import Json.Decode as Decode
import Json.Decode.Field as Field
import Json.Encode as Encode
import Maybe.Extra
import OAuth
import OAuth.Implicit as OAuth
import Process
import Random
import Random.List
import String.Format
import Task
import Time
import Tuple2
import Url exposing (Url)

import App exposing (Flags, State)
import App.Msg as Msg exposing (Msg(..))
import App.Json
import App.UI
import App.Ports as Ports
import Google
import Youtube.Api
import Youtube.PlayerError as PlayerError
import Youtube.PlayerState as PlayerState
import Youtube.Video as Video exposing (Video)


main : Program Flags State Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


init : Flags -> Url -> Navigation.Key -> ( State, Cmd Msg )
init flags url navigationKey =
    let
        urlToken =
            case OAuth.parseToken url of
                OAuth.Empty ->
                    Nothing

                OAuth.Success data ->
                    Maybe.andThen
                        (\dataState ->
                            if convertBytes flags.bytes == dataState then
                                Just
                                    { expires = flags.time + 1000 * Maybe.withDefault 0 data.expiresIn
                                    , scopes = data.scope
                                    , token = data.token
                                    }
                                    |> Maybe.Extra.filter (\t -> t.expires > flags.time)
                            else
                                Nothing
                        )
                        data.state

                OAuth.Error data ->
                    Nothing

        flagsToken =
            Decode.decodeValue App.Json.decodeToken flags.token
                |> Result.toMaybe
                |> Maybe.Extra.filter (\t -> t.expires > flags.time)

        token =
            Maybe.Extra.or urlToken flagsToken
    in
    { navigationKey = navigationKey
    , redirectUri = { url | query = Nothing, fragment = Nothing }
    , time = flags.time
    , token = token
    , lists = Dict.empty
    , messages = []
    , videos = Dict.empty
    , current = 0
    , playlistInStorage = flags.playlistInStorage
    , playlistStorageKey = flags.playlistStorageKey
    , tokenStorageKey = flags.tokenStorageKey
    , oauthClientId = flags.oauthClientId
    , youtubeApiReady = False
    }
        |> Cmd.Extra.withCmd
            ( case token of
                Just t ->
                    Ports.saveToStorage
                        { key = flags.tokenStorageKey
                        , value = App.Json.encodeToken t
                        }
                Nothing ->
                    Ports.removeFromStorage flags.tokenStorageKey
            )


view : State -> Browser.Document Msg
view state =
    { body = [ App.UI.render state ]
    , title = "YouTube Playlist"
    }


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ Ports.onYouTubeApiReady <| always YouTubeApiReady
        , Ports.onPlayerReady <| always PlayerReady
        , Ports.onPlayerStateChange PlayerStateChange
        , Ports.onPlayerError PlayerError
        , Ports.receiveFromStorage ReceiveFromStorage
        , Time.every 1000 SetTime
        , Ports.receiveRandomBytes ReceiveRandomBytes
        , Ports.storageChanged StorageChanged
        , Ports.storageDeleted StorageDeleted
        ]


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        YouTubeApiReady ->
            { state
                | youtubeApiReady = True
            }
                |> Cmd.Extra.withNoCmd

        PlayerReady ->
            playCurrentVideo state

        PlayerStateChange data ->
            let
                decoder =
                    Decode.field "data" PlayerState.decoder
                _ =
                    Decode.decodeValue decoder data
                        |> Debug.log "state"
            in
            case Decode.decodeValue decoder data of
                Ok PlayerState.Ended ->
                    { state
                        | current =
                            if Dict.size state.videos > state.current + 1 then
                                state.current + 1
                            else
                                0
                    }
                        |> playCurrentVideo
                        |> Cmd.Extra.andThen scrollToCurrent
                        |> Cmd.Extra.andThen saveListToStorage

                _ ->
                    state
                        |> Cmd.Extra.withNoCmd

        PlayerError data ->
            Cmd.Extra.withNoCmd state

        SignIn ->
            state
                |> Cmd.Extra.withCmd (Ports.generateRandomBytes 16)

        ReceiveRandomBytes bytes ->
            let
                authorization =
                    { clientId = state.oauthClientId
                    , redirectUri = state.redirectUri
                    , scope =
                        [ Google.oauthScopeYoutube
                        , Google.oauthScopeYoutubeReadOnly
                        ]
                    , state = Just <| convertBytes bytes
                    , url = Google.oauthUrl
                    }
            in
            state
                |> Cmd.Extra.withCmd
                    ( authorization
                        |> OAuth.makeAuthorizationUrl
                        |> Url.toString
                        |> Ports.openPopup
                    )

        ReceiveFromStorage { key, value } ->
            if key == state.playlistStorageKey then
                case Decode.decodeValue App.Json.decodePlaylist value of
                    Ok playlist ->
                        { state
                            | current = playlist.current
                            , videos =
                                playlist.videos
                                    |> List.map App.videoToListItem
                                    |> List.indexedMap Tuple.pair
                                    |> Dict.fromList
                        }
                            |> Cmd.Extra.withCmd (Ports.createPlayer App.UI.playerId)
                            -- |> Cmd.Extra.andThen
                            --     (\s ->
                            --         Process.sleep 100
                            --             |> Task.andThen (always <| Task.succeed NoOp)
                            --             |> Task.perform identity
                            --     )
                            |> Cmd.Extra.andThen scrollToCurrent

                    Err _ ->
                        Cmd.Extra.withNoCmd state -- TODO: Message

            else
                Cmd.Extra.withNoCmd state

        StorageChanged { key, value } ->
            if key == state.tokenStorageKey then
                { state
                    | token =
                        value
                            |> Decode.decodeValue App.Json.decodeToken
                            |> Result.toMaybe
                            |> Maybe.Extra.filter (\t -> t.expires > state.time)
                }
                    |> Cmd.Extra.withCmd (Ports.closePopup ())

            else if key == state.playlistStorageKey then
                { state
                    | playlistInStorage = True
                }
                    |> Cmd.Extra.withNoCmd

            else
                Cmd.Extra.withNoCmd state

        StorageDeleted key ->
            if key == state.tokenStorageKey then
                { state
                    | token = Nothing
                }
                    |> Cmd.Extra.withNoCmd

            else if key == state.playlistStorageKey then
                { state
                    | playlistInStorage = False
                }
                    |> Cmd.Extra.withNoCmd

            else
                Cmd.Extra.withNoCmd state

        SetTime time ->
            { state
                | time = Time.posixToMillis time
            }
                |> checkTokenExpiration

        GetUserPlaylists ->
            { state |
                messages = "Fetching page 1 of user's playlists..." :: state.messages
            }
                |> Cmd.Extra.withCmd
                    ( Youtube.Api.getUserPlaylists
                        state.token
                        Nothing
                        ( GetUserPlaylistsResult 1 [] )
                    )

        GetUserPlaylistsResult pageNo carry result ->
            case result of
                Ok page ->
                    let
                        playlists =
                            List.append carry page.items

                        nextPage =
                            pageNo + 1
                    in
                    if Maybe.Extra.isJust page.nextPageToken then
                        { state
                            | messages = ("Fetching page " ++ (String.fromInt nextPage) ++ "...") :: state.messages
                        }
                            |> Cmd.Extra.withCmd
                                ( Youtube.Api.getUserPlaylists
                                    state.token
                                    page.nextPageToken
                                    ( GetUserPlaylistsResult nextPage playlists )
                                )

                    else
                        { state
                            | messages = ("Fetched " ++ (String.fromInt <| List.length playlists) ++ " playlists.") :: state.messages
                            , lists =
                                List.map
                                    (\list ->
                                        Tuple.pair
                                            list.id
                                            { checked = False
                                            , item = list
                                            }
                                    )
                                    playlists
                                    |> Dict.fromList
                        }
                            |> Cmd.Extra.withNoCmd

                Err err ->
                    { state
                        | messages = "Error when fetching playlists." :: state.messages
                    }
                        |> Cmd.Extra.withCmd (Ports.consoleErr <| httpErrorToString err)

        GetPlaylistVideos andThen playlist ->
            { state |
                messages = ("Fetching page 1 of playlist '" ++ playlist.title ++ "'...") :: state.messages
            }
                |> Cmd.Extra.withCmd
                    ( Youtube.Api.getPlaylistVideos
                        playlist.id
                        state.token
                        Nothing
                        ( GetPlaylistVideosResult andThen playlist 1 [] )
                    )

        GetPlaylistVideosResult andThen playlist pageNo carry result ->
            case result of
                Ok page ->
                    let
                        videos =
                            List.append carry page.items

                        nextPage =
                            pageNo + 1
                    in
                    if Maybe.Extra.isJust page.nextPageToken then
                        { state
                            | messages = ("Fetching page " ++ (String.fromInt nextPage) ++ "...") :: state.messages
                        }
                            |> Cmd.Extra.withCmd
                                ( Youtube.Api.getPlaylistVideos
                                    playlist.id
                                    state.token
                                    page.nextPageToken
                                    ( GetPlaylistVideosResult andThen playlist nextPage videos )
                                )

                    else
                        update
                            (andThen <| Ok videos)
                            { state
                                | messages = ("Fetched " ++ (String.fromInt <| List.length videos) ++ " videos.") :: state.messages
                            }

                Err err ->
                    update
                        (andThen <| Err <| httpErrorToString err)
                        state

        GetAllVideos playlists ->
            case playlists of
                firstPlaylist :: rem ->
                    update
                        ( GetPlaylistVideos
                            ( GetAllVideosResult [] rem firstPlaylist )
                            firstPlaylist
                        )
                        state

                [] ->
                    state
                        |> Cmd.Extra.withNoCmd

        GetAllVideosResult carry remainingPlaylists currentPlaylist result ->
            case result of
                Ok videoList ->
                    let
                        videos =
                            List.append carry videoList
                    in
                    case remainingPlaylists of
                        nextPlaylist :: rem ->
                            update
                                ( GetPlaylistVideos
                                    ( GetAllVideosResult videos rem nextPlaylist )
                                    nextPlaylist
                                )
                                state

                        [] ->
                            state
                                |> Cmd.Extra.withCmd
                                    ( videos
                                        |> Random.List.shuffle
                                        |> Random.generate SetPlaylist
                                    )

                Err err ->
                    { state
                        | messages = "Error when fetching videos." :: state.messages
                    }
                        |> Cmd.Extra.withCmd (Ports.consoleErr err)

        SetPlaylist videos ->
            { state
                | videos =
                    videos
                        |> List.map App.videoToListItem
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                , lists = Dict.empty
                , current = 0
            }
                |> saveListToStorage
                |> Cmd.Extra.addCmd (Ports.createPlayer App.UI.playerId)

        SetListChecked key checked ->
            { state
                | lists =
                    Dict.update
                        key
                        (\maybe ->
                            Maybe.map
                                (\item ->
                                    { item | checked = checked }
                                )
                                maybe
                        )
                        state.lists
            }
                |> Cmd.Extra.withNoCmd

        SetListAll ->
            { state
                | lists =
                    Dict.map
                        (\k v ->
                            { v | checked = True }
                        )
                        state.lists
            }
                |> Cmd.Extra.withNoCmd

        SetListNone ->
            { state
                | lists =
                    Dict.map
                        (\k v ->
                            { v | checked = False }
                        )
                        state.lists
            }
                |> Cmd.Extra.withNoCmd

        LoadListFromStorage ->
            state
                |> Cmd.Extra.withCmd (Ports.loadFromStorage state.playlistStorageKey)

        ToggleEditVideo index bool ->
            { state
                | videos =
                    Dict.update
                        index
                        ( Maybe.map
                            (\listItem ->
                                { listItem
                                    | editOpen = bool
                                    , startAt = secondsToString listItem.video.startAt
                                    , startAtError = Nothing
                                    , endAt = secondsToString listItem.video.endAt
                                    , endAtError = Nothing
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        SetVideoStartAt index string ->
            { state
                | videos =
                    Dict.update
                        index
                        ( Maybe.map
                            (\listItem ->
                                { listItem
                                    | startAt = string
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        SetVideoEndAt index string ->
            { state
                | videos =
                    Dict.update
                        index
                        ( Maybe.map
                            (\listItem ->
                                { listItem
                                    | endAt = string
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        SaveVideoTimes index ->
            case Dict.get index state.videos of
                Just listItem ->
                    case (parseTime listItem.startAt, parseTime listItem.endAt) of
                        (Ok startAt, Ok endAt) ->
                            case validateTimes startAt endAt of
                                Ok _ ->
                                    let
                                        video = listItem.video
                                    in
                                    state
                                        |> Cmd.Extra.withCmd
                                            ( Youtube.Api.updatePlaylistVideo
                                                { video
                                                    | startAt = startAt
                                                    , endAt = endAt
                                                }
                                                state.token
                                                (Debug.log "result" >> always Msg.NoOp)
                                            )

                                Err error ->
                                    -- TODO
                                    state
                                        |> Cmd.Extra.withNoCmd

                        (Err error, _) ->
                            -- TODO
                            state
                                |> Cmd.Extra.withNoCmd

                        (_, Err error) ->
                            -- TODO
                            state
                                |> Cmd.Extra.withNoCmd

                Nothing ->
                    state
                        |> Cmd.Extra.withNoCmd

        ValidateVideoStartAt index ->
            case Dict.get index state.videos of
                Just listItem ->
                    let
                        (startAt, startAtError) =
                            case parseTime listItem.startAt of
                                Ok seconds ->
                                    (secondsToString seconds, Nothing)

                                Err error ->
                                    (listItem.startAt, Just error)
                    in
                    { state
                        | videos =
                            Dict.update
                                index
                                ( Maybe.map
                                    (\listItem_ ->
                                        { listItem_
                                            | startAt = startAt
                                            , startAtError = startAtError
                                        }
                                    )
                                )
                                state.videos
                    }
                        |> Cmd.Extra.withNoCmd

                Nothing ->
                    state
                        |> Cmd.Extra.withNoCmd

        ValidateVideoEndAt index ->
            case Dict.get index state.videos of
                Just listItem ->
                    let
                        (endAt, endAtError) =
                            case parseTime listItem.endAt of
                                Ok seconds ->
                                    (secondsToString seconds, Nothing)

                                Err error ->
                                    (listItem.endAt, Just error)
                    in
                    { state
                        | videos =
                            Dict.update
                                index
                                ( Maybe.map
                                    (\listItem_ ->
                                        { listItem_
                                            | endAt = endAt
                                            , endAtError = endAtError
                                        }
                                    )
                                )
                                state.videos
                    }
                        |> Cmd.Extra.withNoCmd

                Nothing ->
                    state
                        |> Cmd.Extra.withNoCmd

        PlayVideo index ->
            case Dict.get index state.videos of
                Just listItem ->
                    { state
                        | current = index
                    }
                        |> playCurrentVideo
                        |> Cmd.Extra.andThen saveListToStorage

                Nothing ->
                    state
                        |> Cmd.Extra.withNoCmd


checkTokenExpiration : State -> ( State, Cmd msg )
checkTokenExpiration state =
    case state.token of
        Just token ->
            if token.expires < state.time then
                state
                    |> Cmd.Extra.withCmd (Ports.removeFromStorage state.tokenStorageKey)

            else
                Cmd.Extra.withNoCmd state

        Nothing ->
            Cmd.Extra.withNoCmd state


parseTime : String -> Result String (Maybe Int)
parseTime string =
    if string == "" then
        Ok Nothing

    else
        case String.split ":" string of
            [ minuteString, secondString ] ->
                case (String.toInt minuteString, String.toInt secondString) of
                    (Just minutes, Just seconds) ->
                        Ok <| Just <| minutes * 60 + seconds

                    _ ->
                        Err "Invalid time format."

            [ secondString ] ->
                case String.toInt secondString of
                    Just seconds ->
                        Ok <| Just seconds

                    Nothing ->
                        Err "Invalid time format."

            _ ->
                Err "Invalid time format."


validateTimes : Maybe Int -> Maybe Int -> Result String ()
validateTimes maybeStartAt maybeEndAt =
    case (maybeStartAt, maybeEndAt) of
        (Just startAt, Just endAt) ->
            if startAt < 0 then
                Err "Start time cannot be negative."

            else if endAt < 0 then
                Err "End time cannot be negative."

            else if startAt >= endAt then
                Err "Start time cannot be greater than end time."

            else
                Ok ()

        (Just startAt, Nothing) ->
            if startAt < 0 then
                Err "Start time cannot be negative."

            else
                Ok ()

        (Nothing, Just endAt) ->
            if endAt < 0 then
                Err "End time cannot be negative."

            else
                Ok ()

        (Nothing, Nothing) ->
            Ok ()


secondsToString : Maybe Int -> String
secondsToString maybe =
    case maybe of
        Just seconds ->
            "{{ }}:{{}}"
                |> String.Format.value (String.fromInt <| seconds // 60)
                |> String.Format.value (String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds)

        Nothing ->
            ""


saveListToStorage : State -> ( State, Cmd msg )
saveListToStorage state =
    let
        videos =
            state.videos
                |> Dict.values
                |> List.map .video
    in
    state
        |> Cmd.Extra.withCmd
            ( Ports.saveToStorage
                { key = state.playlistStorageKey
                , value = App.Json.encodePlaylist state.current videos
                }
            )


scrollToCurrent : State -> ( State, Cmd Msg )
scrollToCurrent state =
    state
        |> Cmd.Extra.withCmd
            ( Task.map3
                (\video playlist playlistViewport ->
                    playlistViewport.viewport.y + video.element.y - playlist.element.y
                )
                (Browser.Dom.getElement (App.UI.playlistVideoId state.current))
                (Browser.Dom.getElement App.UI.playlistId)
                (Browser.Dom.getViewportOf App.UI.playlistId)
                |> Task.andThen (\y -> Browser.Dom.setViewportOf App.UI.playlistId 0 y)
                |> Task.attempt (always NoOp)
            )


playCurrentVideo : State -> ( State, Cmd msg )
playCurrentVideo state =
    case Dict.get state.current state.videos of
        Just video ->
            state
                |> Cmd.Extra.withCmd
                    ( Ports.playVideo
                        { videoId = video.video.id
                        , startSeconds = Maybe.withDefault -1 video.video.startAt
                        , endSeconds = Maybe.withDefault -1 video.video.endAt
                        }
                    )

        Nothing ->
            state
                |> Cmd.Extra.withNoCmd


convertBytes : List Int -> String
convertBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode >> Base64.bytes >> Base64.encode


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus status ->
            "Failed with status: " ++ (String.fromInt status)

        Http.BadBody string ->
            "Error when parsing response: " ++ string
