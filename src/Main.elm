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
import Url.Parser exposing ((</>))
import Url.Parser.Query

import App exposing (Flags, State)
import App.Msg as Msg exposing (Msg)
import App.UI
import App.Ports as Ports
import Google
import Youtube.Api
import Youtube.PlayerError as PlayerError
import Youtube.PlayerState as PlayerState
import Youtube.Playlist as Playlist exposing (Playlist)
import Youtube.Video as Video exposing (Video)


main : Program Flags State Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Msg.NoOp
        , onUrlChange = \_ -> Msg.NoOp
        }


init : Flags -> Url -> Navigation.Key -> ( State, Cmd Msg )
init flags url navigationKey =
    let
        oauthResult =
            OAuth.parseToken url

        token =
            getToken flags oauthResult
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
    , theme = App.defaultTheme
    , oauthResult = oauthResult
    , playlistsByUrl = ""
    , playlistsByChannel = ""
    }
        |> Cmd.Extra.withCmd
            ( case token of
                Just t ->
                    Ports.saveToStorage
                        { key = flags.tokenStorageKey
                        , value = App.encodeToken t
                        }

                Nothing ->
                    Ports.removeFromStorage flags.tokenStorageKey
            )


getToken : Flags -> OAuth.AuthorizationResult -> Maybe App.Token
getToken flags oauthResult =
    let
        urlToken =
            case oauthResult of
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
            flags.token
                |> Decode.decodeValue App.decodeToken
                |> Result.toMaybe
                |> Maybe.Extra.filter (\t -> t.expires > flags.time)
    in
    Maybe.Extra.or urlToken flagsToken


view : State -> Browser.Document Msg
view state =
    { body = [ App.UI.render state ]
    , title = "YouTube Playlist"
    }


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.batch
        [ Ports.onYouTubeApiReady (\_ -> Msg.Player <| Msg.YouTubeApiReady)
        , Ports.onPlayerReady (\_ -> Msg.Player <| Msg.PlayerReady)
        , Ports.onPlayerStateChange (Msg.Player << Msg.PlayerStateChange)
        , Ports.onPlayerError (Msg.Player << Msg.PlayerError)
        , Ports.receiveRandomBytes (Msg.OAuth << Msg.ReceiveRandomBytes)
        , Ports.receiveFromStorage (Msg.Storage << Msg.ReceiveFromStorage)
        , Ports.storageChanged (Msg.Storage << Msg.StorageChanged)
        , Ports.storageDeleted (Msg.Storage << Msg.StorageDeleted)
        , Time.every 1000 Msg.SetTime
        ]


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Msg.NoOp ->
            Cmd.Extra.withNoCmd state

        Msg.SetTime time ->
            { state
                | time = Time.posixToMillis time
            }
                |> checkTokenExpiration

        Msg.OAuth oauthMsg ->
            updateOAuth oauthMsg state

        Msg.Player playerMsg ->
            updatePlayer playerMsg state

        Msg.PlaylistList playlistListMsg ->
            updatePlaylistList playlistListMsg state

        Msg.Storage storageMsg ->
            updateStorage storageMsg state

        Msg.VideoList videoListMsg ->
            updateVideoList videoListMsg state


updateOAuth : Msg.OAuthMsg -> State -> ( State, Cmd Msg )
updateOAuth msg state =
    case msg of
        Msg.ReceiveRandomBytes bytes ->
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

        Msg.SignIn ->
            state
                |> Cmd.Extra.withCmd (Ports.generateRandomBytes 16)


updatePlayer : Msg.PlayerMsg -> State -> ( State, Cmd Msg )
updatePlayer msg state =
    case msg of
        Msg.PlayNext ->
            { state
                | current = App.nextIndex state
            }
                |> playCurrentVideo
                |> Cmd.Extra.andThen saveListToStorage
                |> Cmd.Extra.andThen scrollToCurrent

        Msg.PlayPrevious ->
            { state
                | current = App.previousIndex state
            }
                |> playCurrentVideo
                |> Cmd.Extra.andThen saveListToStorage
                |> Cmd.Extra.andThen scrollToCurrent

        Msg.PlayerError data ->
            Cmd.Extra.withNoCmd state

        Msg.PlayerReady ->
            playCurrentVideo state

        Msg.PlayerStateChange data ->
            let
                decoder =
                    Decode.field "data" PlayerState.decoder
            in
            case Decode.decodeValue decoder data of
                Ok PlayerState.Ended ->
                    { state
                        | current = App.nextIndex state
                    }
                        |> playCurrentVideo
                        |> Cmd.Extra.andThen scrollToCurrent
                        |> Cmd.Extra.andThen saveListToStorage

                _ ->
                    state
                        |> Cmd.Extra.withNoCmd

        Msg.YouTubeApiReady ->
            { state
                | youtubeApiReady = True
            }
                |> Cmd.Extra.withNoCmd


updatePlaylistList : Msg.PlaylistListMsg -> State -> ( State, Cmd Msg )
updatePlaylistList msg state =
    case msg of
        Msg.GetPlaylistVideos playlists ->
            case playlists of
                firstPlaylist :: remaining ->
                    fetchPlaylistPage
                        []
                        remaining
                        firstPlaylist
                        1
                        Nothing
                        { state
                            | lists = Dict.empty
                        }

                [] ->
                    Cmd.Extra.withNoCmd state

        Msg.GetPlaylistVideosResult videoListItems remainingPlaylists currentPlaylist currentPage result ->
            case result of
                Ok page ->
                    let
                        newVideoListItems =
                            page.items
                                |> List.map (App.makeVideoListItem currentPlaylist)
                                |> List.append videoListItems
                    in
                    case ( page.nextPageToken, remainingPlaylists) of
                        ( Just nextPageToken, _ ) ->
                            fetchPlaylistPage
                                newVideoListItems
                                remainingPlaylists
                                currentPlaylist
                                (currentPage + 1)
                                page.nextPageToken
                                state

                        ( Nothing, nextPlaylist :: nextRemaining ) ->
                            fetchPlaylistPage
                                newVideoListItems
                                nextRemaining
                                nextPlaylist
                                1
                                Nothing
                                state

                        ( Nothing, [] ) ->
                            state
                                |> appendMessage
                                    ("Fetched {{}} videos."
                                        |> String.Format.value (String.fromInt <| List.length newVideoListItems)
                                    )
                                |> Cmd.Extra.withCmd
                                    (newVideoListItems
                                        |> Random.List.shuffle
                                        |> Random.generate (Msg.PlaylistList << Msg.SetPlaylist)
                                    )

                Err err ->
                    state
                        |> appendMessage "Error when fetching videos."
                        |> Cmd.Extra.withCmd (Ports.consoleErr <| httpErrorToString err)

        Msg.GetPlaylistsResult pageNo carry result ->
            case result of
                Ok page ->
                    let
                        playlists =
                            List.append carry page.items

                        nextPage =
                            pageNo + 1

                        playlistIds =
                            state.lists
                                |> Dict.values
                                |> List.map .playlist
                                |> List.map .id
                    in
                    if Maybe.Extra.isJust page.nextPageToken then
                        state
                            |> appendMessage
                                ("Fetching page {{}}..."
                                    |> String.Format.value (String.fromInt nextPage)
                                )
                            |> Cmd.Extra.withCmd
                                (Youtube.Api.getUserPlaylists
                                    state.token
                                    page.nextPageToken
                                    (Msg.PlaylistList << Msg.GetPlaylistsResult nextPage playlists)
                                )

                    else
                        { state
                            | lists =
                                playlists
                                    |> List.map
                                        (\playlist ->
                                            Tuple.pair
                                                playlist.id
                                                { checked = False
                                                , playlist = playlist
                                                }
                                        )
                                    |> Dict.fromList
                                    |> Dict.union state.lists
                        }
                            |> appendMessage
                                ("Fetched {{}} playlists."
                                    |> String.Format.value (String.fromInt <| List.length playlists)
                                )
                            |> Cmd.Extra.withNoCmd

                Err err ->
                    state
                        |> appendMessage "Error when fetching playlists."
                        |> Cmd.Extra.withCmd (Ports.consoleErr <| httpErrorToString err)

        Msg.GetUserPlaylists ->
            { state |
                messages = "Fetching page 1 of user's playlists..." :: state.messages
            }
                |> Cmd.Extra.withCmd
                    (Youtube.Api.getUserPlaylists
                        state.token
                        Nothing
                        (Msg.PlaylistList << Msg.GetPlaylistsResult 1 [])
                    )

        Msg.LoadListFromStorage ->
            state
                |> Cmd.Extra.withCmd (Ports.loadFromStorage state.playlistStorageKey)

        Msg.LoadPlaylistsByChannel ->
            let
                channelId =
                    state.playlistsByChannel
                        |> Url.fromString
                        |> Maybe.andThen
                            (Url.Parser.parse
                                (Url.Parser.s "channel" </> Url.Parser.string)
                            )
                        |> Maybe.withDefault state.playlistsByChannel
            in
            { state |
                messages = "Fetching page 1 of channel playlists..." :: state.messages
            }
                |> Cmd.Extra.withCmd
                    (Youtube.Api.getPlaylistsByChannel
                        channelId
                        state.token
                        Nothing
                        (Msg.PlaylistList << Msg.GetPlaylistsResult 1 [])
                    )

        Msg.LoadPlaylistsByUrl ->
            let
                playlistIds =
                    state.playlistsByUrl
                        |> String.words
                        |> List.map
                            (\string ->
                                string
                                    |> Url.fromString
                                    |> Maybe.andThen
                                        (\url ->
                                            Url.Parser.parse
                                                (Url.Parser.query <| Url.Parser.Query.string "list")
                                                { url | path = "" }
                                        )
                                    |> Maybe.Extra.join
                                    |> Maybe.withDefault string

                            )
            in
            { state |
                messages = "Fetching page 1 of entered playlists..." :: state.messages
            }
                |> Cmd.Extra.withCmd
                    (Youtube.Api.getPlaylistsByIds
                        playlistIds
                        state.token
                        Nothing
                        (Msg.PlaylistList << Msg.GetPlaylistsResult 1 [])
                    )

        Msg.SetChecked key checked ->
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

        Msg.SetCheckedAll ->
            { state
                | lists =
                    Dict.map
                        (\k v ->
                            { v | checked = True }
                        )
                        state.lists
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetCheckedNone ->
            { state
                | lists =
                    Dict.map
                        (\k v ->
                            { v | checked = False }
                        )
                        state.lists
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetPlaylist videos ->
            { state
                | videos =
                    videos
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                , lists = Dict.empty
                , current = 0
            }
                |> saveListToStorage
                |> Cmd.Extra.addCmd (Ports.createPlayer App.UI.playerId)

        Msg.SetPlaylistsByChannel string ->
            { state
                | playlistsByChannel = string
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetPlaylistsByUrl string ->
            { state
                | playlistsByUrl = string
            }
                |> Cmd.Extra.withNoCmd


updateStorage : Msg.StorageMsg -> State -> ( State, Cmd Msg )
updateStorage msg state =
    case msg of
        Msg.ReceiveFromStorage { key, value } ->
            if key == state.playlistStorageKey then
                case Decode.decodeValue App.decodePlaylist value of
                    Ok playlist ->
                        { state
                            | current = playlist.current
                            , videos =
                                playlist.items
                                    |> List.indexedMap Tuple.pair
                                    |> Dict.fromList
                        }
                            |> Cmd.Extra.withCmd (Ports.createPlayer App.UI.playerId)
                            |> Cmd.Extra.addCmd
                                ( Task.perform
                                    (\_ -> Msg.VideoList <| Msg.ScrollToCurrentVideo)
                                    (Process.sleep 10)
                                )

                    Err err ->
                        Cmd.Extra.withNoCmd state -- TODO: Message

            else
                Cmd.Extra.withNoCmd state

        Msg.StorageChanged { key, value } ->
            if key == state.tokenStorageKey then
                { state
                    | token =
                        value
                            |> Decode.decodeValue App.decodeToken
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

        Msg.StorageDeleted key ->
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


updateVideoList : Msg.VideoListMsg -> State -> ( State, Cmd Msg )
updateVideoList msg state =
    case msg of
        Msg.PlayVideo index ->
            case Dict.get index state.videos of
                Just listItem ->
                    { state
                        | current = index
                    }
                        |> playCurrentVideo
                        |> Cmd.Extra.andThen saveListToStorage

                Nothing ->
                    Cmd.Extra.withNoCmd state

        Msg.SaveVideoTimes index ->
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
                                                    , note =
                                                        if String.isEmpty listItem.note then
                                                            Nothing
                                                        else
                                                            Just listItem.note
                                                }
                                                state.token
                                                (Msg.VideoList << Msg.SaveVideoTimesResult index)
                                            )

                                Err error ->
                                    state -- TODO
                                        |> Cmd.Extra.withNoCmd

                        (Err error, _) ->
                            state -- TODO
                                |> Cmd.Extra.withNoCmd

                        (_, Err error) ->
                            state -- TODO
                                |> Cmd.Extra.withNoCmd

                Nothing ->
                    Cmd.Extra.withNoCmd state

        Msg.SaveVideoTimesResult index result ->
            case result of
                Ok video ->
                    { state
                        | videos =
                            Dict.update
                                index
                                ( Maybe.map
                                    (\listItem ->
                                        { listItem
                                            | video = video
                                        }
                                    )
                                )
                                state.videos
                    }
                        |> saveListToStorage

                Err err ->
                    Cmd.Extra.withNoCmd state -- TODO

        Msg.ScrollToCurrentVideo ->
            scrollToCurrent state

        Msg.SetVideoEndAt index string ->
            { state
                | videos =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | endAt = string
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetVideoNote index string ->
            { state
                | videos =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | note = string
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetVideoStartAt index string ->
            { state
                | videos =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | startAt = string
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        Msg.ToggleEditVideo index bool ->
            { state
                | videos =
                    Dict.update
                        index
                        ( Maybe.map
                            (\listItem ->
                                { listItem
                                    | editOpen = bool
                                    , startAt = App.secondsToString listItem.video.startAt
                                    , startAtError = Nothing
                                    , endAt = App.secondsToString listItem.video.endAt
                                    , endAtError = Nothing
                                    , note = Maybe.withDefault "" listItem.video.note
                                }
                            )
                        )
                        state.videos
            }
                |> Cmd.Extra.withNoCmd

        Msg.ValidateVideoEndAt index ->
            case Dict.get index state.videos of
                Just listItem ->
                    let
                        ( endAt, endAtError ) =
                            case parseTime listItem.endAt of
                                Ok seconds ->
                                    ( App.secondsToString seconds, Nothing )

                                Err error ->
                                    ( listItem.endAt, Just error )
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
                    Cmd.Extra.withNoCmd state

        Msg.ValidateVideoStartAt index ->
            case Dict.get index state.videos of
                Just listItem ->
                    let
                        ( startAt, startAtError ) =
                            case parseTime listItem.startAt of
                                Ok seconds ->
                                    ( App.secondsToString seconds, Nothing )

                                Err error ->
                                    ( listItem.startAt, Just error )
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
                    Cmd.Extra.withNoCmd state


appendMessage : String -> State -> State
appendMessage message state =
    { state
        | messages = message :: state.messages
    }


fetchPlaylistPage :
    List App.VideoListItem
    -> List Playlist
    -> Playlist
    -> Int
    -> Maybe String
    -> State
    -> ( State, Cmd Msg )
fetchPlaylistPage videoListItems remainingPlaylists playlist page pageToken state =
    state
        |> appendMessage
            ("Fetching page {{}} of playlist '{{}}'..."
                |> String.Format.value (String.fromInt page)
                |> String.Format.value playlist.title
            )
        |> Cmd.Extra.withCmd
            (Youtube.Api.getPlaylistVideos
                playlist.id
                state.token
                pageToken
                (Msg.PlaylistList << Msg.GetPlaylistVideosResult videoListItems remainingPlaylists playlist page)
            )


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


saveListToStorage : State -> ( State, Cmd msg )
saveListToStorage state =
    { state
        | playlistInStorage = True
    }
        |> Cmd.Extra.withCmd
            ( Ports.saveToStorage
                { key = state.playlistStorageKey
                , value = App.encodePlaylist state.current <| Dict.values state.videos
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
                |> Task.attempt (\_ -> Msg.NoOp)
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
