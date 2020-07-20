module Main exposing (main)

import Base64.Encode as Base64
import Browser
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
import String.Format
import Time
import Tuple2
import Url exposing (Url)

import App exposing (Flags, State)
import App.Msg as Msg exposing (Msg(..))
import App.Json
import App.UI
import App.Ports as Ports
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
        redirectUri =
            { url | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)

        urlToken =
            case OAuth.parseToken url of
                OAuth.Empty ->
                    Nothing

                OAuth.Success data ->
                    case data.state of
                        Just dataState ->
                            if convertBytes flags.bytes == dataState then
                                Just
                                    { token = data.token
                                    , expires = flags.time + 1000 * Maybe.withDefault 0 data.expiresIn
                                    }
                            else
                                Nothing

                        _ ->
                            Nothing

                OAuth.Error data ->
                    Nothing

        flagsToken =
            Decode.decodeValue App.Json.decodeToken flags.token
                |> Result.toMaybe
                |> Maybe.Extra.filter (\t -> t.expires > flags.time)

        token =
            Maybe.Extra.or flagsToken urlToken

        videos =
            flags.storedList.value
                |> Decode.decodeValue (Decode.list Video.decoder)
                |> Result.withDefault []
                |> List.map App.videoToListItem
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
    in
    { navigationKey = navigationKey
    , redirectUri = redirectUri
    , time = flags.time
    , token = token
    , lists = Dict.empty
    , messages = []
    , videos = videos
    , current = 0
    }
        |> Cmd.Extra.withCmd
            clearUrl
        |> Cmd.Extra.addCmd
            ( case token of
                Just t ->
                    Ports.saveToStorage
                        { key = "token"
                        , value = App.Json.encodeToken t
                        }
                Nothing ->
                    Ports.removeFromStorage "token"
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
        ]


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        YouTubeApiReady ->
            state
                |> Cmd.Extra.withNoCmd

        PlayerReady ->
            playVideo state

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
                        | current = state.current + 1
                    }
                        |> playVideo

                _ ->
                    state
                        |> Cmd.Extra.withNoCmd

        PlayerError data ->
            ( state, Cmd.none )

        SignIn ->
            state
                |> Cmd.Extra.withCmd (Ports.generateRandomBytes 16)

        ReceiveRandomBytes bytes ->
            let
                authorization =
                    { clientId = "1004146990872-svm4c3j6nof3afhjbjsf4mask09kc85n.apps.googleusercontent.com"
                    , redirectUri = state.redirectUri
                    , scope =
                        -- [ "https://www.googleapis.com/auth/youtube.readonly"
                        -- ]
                        [ "https://www.googleapis.com/auth/youtube"
                        ]
                    , state = Just <| convertBytes bytes
                    , url = { emptyUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
                    }
            in
            state
                |> Cmd.Extra.withCmd
                    ( authorization
                        |> OAuth.makeAuthorizationUrl
                        |> Url.toString
                        |> Navigation.load
                    )

        ReceiveFromStorage { key, value } ->
            let
                _ =
                    Decode.decodeValue Decode.string value
                        |> Debug.log "value"
            in
            ( state, Cmd.none )

        SetTime time ->
            { state | time = Time.posixToMillis time }
                |> Cmd.Extra.withNoCmd

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
                            { state
                                | videos =
                                    videos
                                        |> List.map App.videoToListItem
                                        |> List.indexedMap Tuple.pair
                                        |> Dict.fromList
                            }
                                |> Cmd.Extra.withCmd (saveListToStorage videos)

                Err err ->
                    { state
                        | messages = "Error when fetching videos." :: state.messages
                    }
                        |> Cmd.Extra.withCmd (Ports.consoleErr err)

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

        PlayList ->
            state
                |> Cmd.Extra.withCmd (Ports.createPlayer App.UI.playerId)

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
            if seconds >= 60 then
                "{{ }}:{{}}"
                    |> String.Format.value (String.fromInt <| seconds // 60)
                    |> String.Format.value (String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds)
            else
                String.fromInt seconds

        Nothing ->
            ""


saveListToStorage : List Video -> Cmd msg
saveListToStorage videos =
    { key = "list"
    , value = Encode.list Video.encode videos
    }
        |> Ports.saveToStorage


playVideo : State -> ( State, Cmd msg )
playVideo state =
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


emptyUrl : Url
emptyUrl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


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
