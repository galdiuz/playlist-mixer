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
import Google.OAuth
import Google.OAuth.Scope
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
init flags url _ =
    let
        oauthResult =
            OAuth.parseToken url

        token =
            getToken flags oauthResult
    in
    { autoplay = True
    , currentListIndex = 0
    , currentVideoIndex = 0
    , messages = []
    , oauthClientId = flags.oauthClientId
    , oauthResult = oauthResult
    , oauthScopes = []
    , playlistInStorage = flags.playlistInStorage
    , playlistList = Dict.empty
    , playlistStorageKey = flags.playlistStorageKey
    , playlistsByChannelValue = ""
    , playlistsByUrlValue = ""
    , redirectUri = { url | query = Nothing, fragment = Nothing }
    , scrolling = False
    , searchValue = ""
    , theme = App.darkTheme
    , time = flags.time
    , token = token
    , tokenStorageKey = flags.tokenStorageKey
    , videoList = Dict.empty
    , youtubeApiReady = False
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
                                    { email = Nothing
                                    , expires = flags.time + 1000 * Maybe.withDefault 0 data.expiresIn
                                    , scopes = List.filterMap Google.OAuth.Scope.fromString data.scope
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
    , title =
        case Dict.get state.currentVideoIndex state.videoList of
            Just listItem ->
                "Playlist Mixer - " ++ listItem.video.title

            Nothing ->
                "Playlist Mixer"
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

        Msg.SetAutoplay autoplay ->
            { state
                | autoplay = autoplay
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetTime time ->
            { state
                | time = Time.posixToMillis time
            }
                |> checkTokenExpiration

        Msg.SetTheme theme ->
            { state
                | theme = theme
            }
                |> Cmd.Extra.withNoCmd

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
        Msg.GetUserEmailResult result ->
            case (result, state.token) of
                (Ok email, Just token) ->
                    let
                        newToken =
                            { token
                                | email = Just email
                            }
                    in
                    { state
                        | token = Just newToken
                    }
                        |> Cmd.Extra.withCmd
                            (Ports.saveToStorage
                                { key = state.tokenStorageKey
                                , value = App.encodeToken newToken
                                }
                            )

                _ ->
                    Cmd.Extra.withNoCmd state

        Msg.ReceiveRandomBytes bytes ->
            let
                authorization =
                    { clientId = state.oauthClientId
                    , redirectUri = state.redirectUri
                    , scope =
                        state.oauthScopes
                            |> List.append
                                [ Google.OAuth.Scope.Email
                                ]
                            |> List.map Google.OAuth.Scope.toString
                    , state = Just <| convertBytes bytes
                    , url = Google.OAuth.url
                    }
            in
            state
                |> Cmd.Extra.withCmd
                    ( authorization
                        |> OAuth.makeAuthorizationUrl
                        |> Url.toString
                        |> Ports.openPopup
                    )

        Msg.SignIn scopes ->
            { state
                | oauthScopes = scopes
            }
                |> Cmd.Extra.withCmd (Ports.generateRandomBytes 16)

        Msg.SignOut ->
            state
                |> Cmd.Extra.withCmd (Ports.removeFromStorage state.tokenStorageKey)


updatePlayer : Msg.PlayerMsg -> State -> ( State, Cmd Msg )
updatePlayer msg state =
    case msg of
        Msg.PlayNext ->
            playNextVideo state

        Msg.PlayPrevious ->
            { state
                | currentVideoIndex = App.previousIndex state
            }
                |> updateCurrentListIndex
                |> playCurrentVideo
                |> Cmd.Extra.andThen saveListToStorage
                |> Cmd.Extra.andThen scrollListToCurrent

        Msg.PlayerError value ->
            case Decode.decodeValue (Decode.field "data" PlayerError.decoder) value of
                Ok error ->
                    { state
                        | videoList =
                            Dict.update
                                state.currentVideoIndex
                                (Maybe.map
                                    (\listItem ->
                                        { listItem
                                            | error = Just error
                                        }
                                    )
                                )
                                state.videoList
                    }
                        |> (if state.autoplay then
                                playNextVideo
                            else
                                Cmd.Extra.withNoCmd
                           )

                _ ->
                    Cmd.Extra.withNoCmd state

        Msg.PlayerReady ->
            playCurrentVideo state

        Msg.PlayerStateChange value ->
            case Decode.decodeValue (Decode.field "data" PlayerState.decoder) value of
                Ok PlayerState.Ended ->
                    if state.autoplay then
                        playNextVideo state
                    else
                        Cmd.Extra.withNoCmd state

                _ ->
                    Cmd.Extra.withNoCmd state

        Msg.YouTubeApiReady ->
            { state
                | youtubeApiReady = True
            }
                |> Cmd.Extra.withNoCmd


updatePlaylistList : Msg.PlaylistListMsg -> State -> ( State, Cmd Msg )
updatePlaylistList msg state =
    case msg of
        Msg.AppendPlaylist videos ->
            let
                videoIsInList listItem =
                    state.videoList
                        |> Dict.values
                        |> List.map (.video >> .id)
                        |> List.member listItem.video.id
            in
            { state
                | videoList =
                    videos
                        |> List.filter (not << videoIsInList)
                        |> List.indexedMap Tuple.pair
                        |> List.map (Tuple.mapFirst ((+) (Dict.size state.videoList)))
                        |> Dict.fromList
                        |> Dict.union state.videoList
                , playlistList = Dict.empty
            }
                |> saveListToStorage

        Msg.GetPlaylistVideos andThen ->
            let
                playlists =
                    state.playlistList
                        |> Dict.values
                        |> List.filterMap
                            (\listItem ->
                                if listItem.checked then
                                    Just listItem.playlist
                                else
                                    Nothing
                            )
            in
            case playlists of
                firstPlaylist :: remaining ->
                    fetchPlaylistPage
                        andThen
                        []
                        remaining
                        firstPlaylist
                        1
                        Nothing
                        { state
                            | playlistList = Dict.empty
                        }

                [] ->
                    Cmd.Extra.withNoCmd state

        Msg.GetPlaylistVideosResult andThen videoListItems remainingPlaylists currentPlaylist currentPage result ->
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
                                andThen
                                newVideoListItems
                                remainingPlaylists
                                currentPlaylist
                                (currentPage + 1)
                                page.nextPageToken
                                state

                        ( Nothing, nextPlaylist :: nextRemaining ) ->
                            fetchPlaylistPage
                                andThen
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
                                        |> Random.generate andThen
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
                            | playlistList =
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
                                    |> Dict.union state.playlistList
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
                    state.playlistsByChannelValue
                        |> Url.fromString
                        |> Maybe.andThen
                            (Url.Parser.parse
                                (Url.Parser.s "channel" </> Url.Parser.string)
                            )
                        |> Maybe.withDefault state.playlistsByChannelValue
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
                    state.playlistsByUrlValue
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
                | playlistList =
                    Dict.update
                        key
                        (\maybe ->
                            Maybe.map
                                (\item ->
                                    { item | checked = checked }
                                )
                                maybe
                        )
                        state.playlistList
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetCheckedAll ->
            { state
                | playlistList =
                    Dict.map
                        (\k v ->
                            { v | checked = True }
                        )
                        state.playlistList
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetCheckedNone ->
            { state
                | playlistList =
                    Dict.map
                        (\k v ->
                            { v | checked = False }
                        )
                        state.playlistList
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetPlaylist videos ->
            { state
                | videoList =
                    videos
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                , playlistList = Dict.empty
                , currentVideoIndex = 0
            }
                |> saveListToStorage
                |> Cmd.Extra.addCmd (Ports.createPlayer App.UI.playerId)

        Msg.SetPlaylistsByChannel string ->
            { state
                | playlistsByChannelValue = string
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetPlaylistsByUrl string ->
            { state
                | playlistsByUrlValue = string
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
                            | currentVideoIndex = playlist.current
                            , videoList =
                                playlist.items
                                    |> List.indexedMap Tuple.pair
                                    |> Dict.fromList
                        }
                            |> updateCurrentListIndex
                            |> Cmd.Extra.withCmd (Ports.createPlayer App.UI.playerId)
                            |> Cmd.Extra.addCmd
                                (Task.perform
                                    (\_ -> Msg.VideoList <| Msg.ScrollToCurrentVideo)
                                    (Process.sleep 10)
                                )

                    Err err ->
                        Cmd.Extra.withNoCmd state -- TODO: Message

            else
                Cmd.Extra.withNoCmd state

        Msg.StorageChanged { key, value } ->
            if key == state.tokenStorageKey then
                let
                    token =
                        value
                            |> Decode.decodeValue App.decodeToken
                            |> Result.toMaybe
                            |> Maybe.Extra.filter (\t -> t.expires > state.time)
                in
                { state
                    | token = token
                }
                    |> Cmd.Extra.withCmd (Ports.closePopup ())
                    |> Cmd.Extra.addCmd
                        (case Maybe.map .email token of
                            Just Nothing ->
                                Youtube.Api.getUserEmail
                                    token
                                    (Msg.OAuth << Msg.GetUserEmailResult)

                            _ ->
                                Cmd.none
                        )

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
            case Dict.get index state.videoList of
                Just listItem ->
                    { state
                        | currentVideoIndex = index
                    }
                        |> playCurrentVideo
                        |> Cmd.Extra.andThen saveListToStorage

                Nothing ->
                    Cmd.Extra.withNoCmd state

        Msg.SaveVideoTimes index ->
            case Dict.get index state.videoList of
                Just listItem ->
                    case ( parseTime listItem.startAtValue, parseTime listItem.endAtValue ) of
                        ( Ok startAt, Ok endAt ) ->
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

                        _ ->
                            Cmd.Extra.withNoCmd state

                Nothing ->
                    Cmd.Extra.withNoCmd state

        Msg.SaveVideoTimesResult index result ->
            case result of
                Ok video ->
                    { state
                        | videoList =
                            Dict.update
                                index
                                (Maybe.map
                                    (\listItem ->
                                        { listItem
                                            | video = video
                                            , editOpen = False
                                        }
                                    )
                                )
                                state.videoList
                    }
                        |> saveListToStorage

                Err err ->
                    Cmd.Extra.withNoCmd state -- TODO

        Msg.Scroll scroll ->
            let
                scrollOffset =
                    50

                isSearching =
                    not <| String.isEmpty state.searchValue

                isEarlierAvailable =
                    state.currentListIndex > 0

                isNearTop =
                    round scroll.scrollTop < scrollOffset

                isLaterAvailable =
                    state.currentListIndex < Dict.size state.videoList - App.videoListPageSize

                isNearBottom =
                    (round scroll.scrollTop) + scroll.containerHeight > scroll.contentHeight - scrollOffset
            in
            if not state.scrolling && not isSearching && isEarlierAvailable && isNearTop then
                { state
                    | scrolling = True
                }
                    |> Cmd.Extra.withCmd
                        (Browser.Dom.getElement
                            (App.UI.videoListVideoId state.currentListIndex)
                            |> Task.map (.element >> .y)
                            |> Task.attempt
                                (Result.map (Msg.VideoList << Msg.ScrollEarlier)
                                    >> Result.withDefault Msg.NoOp
                                )
                        )
            else if not state.scrolling && not isSearching && isLaterAvailable && isNearBottom then
                { state
                    | scrolling = True
                }
                    |> Cmd.Extra.withCmd
                        (Browser.Dom.getElement
                            (App.UI.videoListVideoId <| state.currentListIndex + App.videoListPageSize - 1)
                            |> Task.map (.element >> .y)
                            |> Task.attempt
                                (Result.map (Msg.VideoList << Msg.ScrollLater)
                                    >> Result.withDefault Msg.NoOp
                                )
                        )
            else
                Cmd.Extra.withNoCmd state

        Msg.ScrollEarlier prevY ->
            { state
                | currentListIndex =
                    state.currentListIndex - App.videoListScrollSize
                        |> Basics.max 0
            }
                |> Cmd.Extra.withCmd
                    (Task.map2
                        (\video videoListViewport ->
                            videoListViewport.viewport.y - prevY + video.element.y
                        )
                        (Browser.Dom.getElement (App.UI.videoListVideoId state.currentListIndex))
                        (Browser.Dom.getViewportOf App.UI.videoListId)
                        |> Task.andThen (\y -> Browser.Dom.setViewportOf App.UI.videoListId 0 y)
                        |> Task.attempt (\_ -> Msg.VideoList Msg.SetScrolling)
                    )

        Msg.ScrollLater prevY ->
            { state
                | currentListIndex =
                    state.currentListIndex + App.videoListScrollSize
                        |> Basics.min (Dict.size state.videoList - App.videoListPageSize)
            }
                |> Cmd.Extra.withCmd
                    (Task.map2
                        (\video videoListViewport ->
                            videoListViewport.viewport.y - prevY + video.element.y
                        )
                        (Browser.Dom.getElement
                            (App.UI.videoListVideoId <| state.currentListIndex + App.videoListPageSize - 1)
                        )
                        (Browser.Dom.getViewportOf App.UI.videoListId)
                        |> Task.andThen (\y -> Browser.Dom.setViewportOf App.UI.videoListId 0 y)
                        |> Task.attempt (\_ -> Msg.VideoList Msg.SetScrolling)
                    )

        Msg.ScrollToCurrentVideo ->
            scrollListToCurrent state

        Msg.SetScrolling ->
            { state
                | scrolling = False
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetSearch value ->
            { state
                | searchValue = value
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetVideoEndAt index string ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | endAtValue = string
                                }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetVideoNote index string ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | note = string
                                }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd

        Msg.SetVideoStartAt index string ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | startAtValue = string
                                }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd

        Msg.ShowCurrentVideo ->
            state
                |> updateCurrentListIndex
                |> scrollListToCurrent

        Msg.ShowEarlierVideos ->
            { state
                | currentListIndex =
                    state.currentListIndex - App.videoListPageSize
                        |> Basics.max 0
            }
                |> scrollListToTop

        Msg.ShowLaterVideos ->
            { state
                | currentListIndex =
                    state.currentListIndex + App.videoListPageSize
                        |> Basics.min (Dict.size state.videoList - App.videoListPageSize)
            }
                |> scrollListToTop

        Msg.ToggleEditVideo index bool ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                { listItem
                                    | editOpen = bool
                                    , startAtError = Nothing
                                    , startAtValue = App.secondsToString listItem.video.startAt
                                    , endAtError = Nothing
                                    , endAtValue = App.secondsToString listItem.video.endAt
                                    , note = Maybe.withDefault "" listItem.video.note
                                }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd

        Msg.ValidateVideoEndAt index ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                case parseTime listItem.endAtValue of
                                    Ok seconds ->
                                        { listItem
                                            | endAtError = Nothing
                                            , endAtValue = App.secondsToString seconds
                                        }

                                    Err error ->
                                        { listItem
                                            | endAtError = Just error
                                            , endAtValue = listItem.endAtValue
                                        }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd


        Msg.ValidateVideoStartAt index ->
            { state
                | videoList =
                    Dict.update
                        index
                        (Maybe.map
                            (\listItem ->
                                case parseTime listItem.startAtValue of
                                    Ok seconds ->
                                        { listItem
                                            | startAtError = Nothing
                                            , startAtValue = App.secondsToString seconds
                                        }

                                    Err error ->
                                        { listItem
                                            | startAtError = Just error
                                            , startAtValue = listItem.startAtValue
                                        }
                            )
                        )
                        state.videoList
            }
                |> Cmd.Extra.withNoCmd


appendMessage : String -> State -> State
appendMessage message state =
    { state
        | messages = message :: state.messages
    }


fetchPlaylistPage :
    (List App.VideoListItem -> Msg)
    -> List App.VideoListItem
    -> List Playlist
    -> Playlist
    -> Int
    -> Maybe String
    -> State
    -> ( State, Cmd Msg )
fetchPlaylistPage andThen videoListItems remainingPlaylists playlist page pageToken state =
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
                (Msg.GetPlaylistVideosResult andThen videoListItems remainingPlaylists playlist page
                    >> Msg.PlaylistList
                )
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

            [ _ ] ->
                if String.length string > 2 then
                    parseTime <| String.dropRight 2 string ++ ":" ++ String.right 2 string
                else
                    case String.toInt string of
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
    state
        |> Cmd.Extra.withCmd
            ( Ports.saveToStorage
                { key = state.playlistStorageKey
                , value = App.encodePlaylist state.currentVideoIndex <| Dict.values state.videoList
                }
            )


scrollListToCurrent : State -> ( State, Cmd Msg )
scrollListToCurrent state =
    state
        |> Cmd.Extra.withCmd
            (Task.map3
                (\video videoList videoListViewport ->
                    videoListViewport.viewport.y - videoList.element.y + video.element.y
                )
                (Browser.Dom.getElement (App.UI.videoListVideoId state.currentVideoIndex))
                (Browser.Dom.getElement App.UI.videoListId)
                (Browser.Dom.getViewportOf App.UI.videoListId)
                |> Task.andThen (\y -> Browser.Dom.setViewportOf App.UI.videoListId 0 y)
                |> Task.attempt (\_ -> Msg.NoOp)
            )


scrollListToTop : State -> ( State, Cmd Msg )
scrollListToTop state =
    state
        |> Cmd.Extra.withCmd
            (Browser.Dom.setViewportOf App.UI.videoListId 0 0
                |> Task.attempt (\_ -> Msg.NoOp)
            )


playCurrentVideo : State -> ( State, Cmd msg )
playCurrentVideo state =
    case Dict.get state.currentVideoIndex state.videoList of
        Just listItem ->
            state
                |> Cmd.Extra.withCmd
                    ( Ports.playVideo
                        { videoId = listItem.video.id
                        , startSeconds = listItem.video.startAt
                        , endSeconds = listItem.video.endAt
                        }
                    )

        Nothing ->
            state
                |> Cmd.Extra.withNoCmd


playNextVideo : State -> ( State, Cmd Msg )
playNextVideo state =
    { state
        | currentVideoIndex = App.nextIndex state
    }
        |> updateCurrentListIndex
        |> playCurrentVideo
        |> Cmd.Extra.andThen saveListToStorage
        |> Cmd.Extra.andThen scrollListToCurrent


updateCurrentListIndex : State -> State
updateCurrentListIndex state =
    { state
        | currentListIndex =
            state.currentVideoIndex - 5
                |> Basics.max 0
                |> Basics.min (Dict.size state.videoList - App.videoListPageSize)
    }


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
