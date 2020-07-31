module App.UI exposing
    ( playlistVideoId
    , playlistId
    , playerId
    , render
    )

import Dict
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
import Html exposing (Html)
import Html.Attributes as HA
import Maybe.Extra
import OAuth
import OAuth.Implicit as OAuth
import String.Format

import App exposing (State)
import App.Msg as Msg exposing (Msg)
import Google
import Youtube.Playlist as Playlist
import Youtube.Video as Video


render : State -> Html Msg
render state =
    El.layout
        [ El.width El.fill
        , El.height El.shrink
        ]
        <| case state.oauthResult of
            OAuth.Empty ->
                El.el
                    [ Background.color state.theme.bg
                    , Font.color state.theme.fg
                    , El.width El.fill
                    , El.height El.fill
                    ]
                    <| El.column
                        [ El.width <| El.maximum 800 El.fill
                        , El.height El.fill
                        , El.centerX
                        , El.spacing 30
                        ]
                        [ renderHeader state
                        , renderPlayer state
                        , renderVideoList state
                        , renderWelcomeMessage state
                        , renderPlaylistMenu state
                        , renderPlaylistList state
                        , renderMessages state
                        , renderFooter state
                        , El.html FontAwesome.Styles.css
                        ]

            OAuth.Success _ ->
                El.paragraph
                    [ El.paddingEach { paddingZero | top = 20 }
                    , El.spacing 10
                    , Font.center
                    ]
                    [ El.text "Successfully signed in. You can now close this window."
                    ]

            OAuth.Error err ->
                El.paragraph
                    [ El.spacing 10
                    ]
                    [ El.text "An error occured during sign in:"
                    , El.text <| OAuth.errorCodeToString err.error
                    , El.text <| Maybe.withDefault "" err.errorDescription
                    ]


renderHeader : State -> Element msg
renderHeader state =
    El.column
        [ El.width El.fill
        ]
        [ El.el
            [ El.paddingEach { paddingZero | top = 30, bottom = 10 }
            , Font.size 32
            , Font.bold
            ]
            <| El.text "YouTube Playlist"
        , renderSpacer state
        ]


renderFooter : State -> Element Msg
renderFooter state =
    El.column
        [ El.width El.fill
        , El.paddingEach { paddingZero | bottom = 10 }
        , El.spacing 10
        ]
        [ renderSpacer state
        , El.row
            [ El.spacing 10
            ]
            [ El.newTabLink
                buttonStyle
                { label = linkLabel "Privacy policy"
                , url = App.privacyPolicyUrl state
                }
            , El.newTabLink
                buttonStyle
                { label = linkLabel "View source on GitHub"
                , url = "https://github.com/galdiuz/youtube-playlist"
                }
            , Input.button
                buttonStyle
                { label = El.text "Light theme"
                , onPress = Just <| Msg.SetTheme App.lightTheme
                }
            , Input.button
                buttonStyle
                { label = El.text "Dark theme"
                , onPress = Just <| Msg.SetTheme App.darkTheme
                }
            ]
        ]


linkLabel : String -> Element msg
linkLabel text =
    El.row
        [ El.spacing 5
        ]
        [ El.text text
        , El.el
            []
            <| El.html <| FontAwesome.Icon.viewIcon FontAwesome.Solid.externalLinkAlt
        ]


renderSpacer : State -> Element msg
renderSpacer state =
    El.el
        [ El.width El.fill
        , El.height <| El.px 1
        , Background.color state.theme.disabled
        ]
        El.none


renderWelcomeMessage : State -> Element msg
renderWelcomeMessage state =
    if Dict.isEmpty state.videos then
        El.paragraph
            []
            [ El.text
                <| "Welcome to YouTube Playlist! This application lets you mix multiple YouTube"
                ++ " playlists together into one, playing them all in one big shuffled list."
            ]
      else
        El.none


renderPlaylistMenu : State -> Element Msg
renderPlaylistMenu state =
    El.column
        [ El.spacing 30
        , El.width El.fill
        ]
        [ if state.playlistInStorage && Dict.isEmpty state.videos then
            El.column
                [ El.spacing 10
                ]
                [ El.el
                    [ Font.size 24
                    ]
                    <| El.text "Resume from previous list"
                , Input.button
                    buttonStyle
                    { onPress = Just <| Msg.PlaylistList <| Msg.LoadListFromStorage
                    , label = El.text "Resume"
                    }
                ]
          else
            El.none

        , if Maybe.Extra.isJust state.token then
            El.column
                [ El.spacing 30
                , El.width El.fill
                ]
                [ El.column
                    [ El.spacing 10
                    ]
                    [ El.el
                        [ Font.size 24
                        ]
                        <| El.text "Load playlists from your YouTube account"
                    , Input.button
                        buttonStyle
                        { onPress = Just <| Msg.PlaylistList <| Msg.GetUserPlaylists
                        , label = El.text "Load"
                        }
                    ]
                , El.column
                    [ El.spacing 10
                    , El.width El.fill
                    ]
                    [ El.el
                        [ Font.size 24
                        ]
                        <| El.text "Load playlists by URL or ID"
                    , Input.multiline
                        [ Background.color state.theme.bg
                        , El.width El.fill
                        ]
                        { label = Input.labelHidden ""
                        , onChange = Msg.PlaylistList << Msg.SetPlaylistsByUrl
                        , placeholder =
                            Input.placeholder
                                []
                                (El.text "https://www.youtube.com/playlist?list=aaabbbccc")
                                |> Just
                        , spellcheck = False
                        , text = state.playlistsByUrl
                        }
                    , if String.isEmpty state.playlistsByUrl then
                        El.el
                            (disabledButtonStyle state)
                            <| El.text "Load"
                      else
                        Input.button
                            buttonStyle
                            { onPress = Just <| Msg.PlaylistList <| Msg.LoadPlaylistsByUrl
                            , label = El.text "Load"
                            }
                    ]
                , El.column
                    [ El.spacing 10
                    , El.width El.fill
                    ]
                    [ El.el
                        [ Font.size 24
                        ]
                        <| El.text "Load playlists by channel URL or ID"
                    , Input.text
                        [ Background.color state.theme.bg
                        ]
                        { label = Input.labelHidden ""
                        , onChange = Msg.PlaylistList << Msg.SetPlaylistsByChannel
                        , placeholder =
                            Input.placeholder
                                []
                                (El.text "https://www.youtube.com/channel/aaabbbccc")
                                |> Just
                        , text = state.playlistsByChannel
                        }
                    , if String.isEmpty state.playlistsByChannel then
                        El.el
                            (disabledButtonStyle state)
                            <| El.text "Load"
                      else
                        Input.button
                            buttonStyle
                            { onPress = Just <| Msg.PlaylistList <| Msg.LoadPlaylistsByChannel
                            , label = El.text "Load"
                            }
                    ]
                ]

          else
            El.column
                [ El.spacing 10
                ]
                [ El.paragraph
                    []
                    [ El.text
                        <| "Usage of YouTube's APIs requires sign in to a Google account. Allowing"
                        ++ " the app to read or manage your YouTube account is optional, and is only"
                        ++ " required if you want to access your private playlists. For more"
                        ++ " information, refer to "
                    , El.newTabLink
                        [ Font.underline
                        ]
                        { label = El.text "YouTube Playlist's privacy policy"
                        , url = App.privacyPolicyUrl state
                        }
                    , El.text "."
                    ]
                , Input.button
                    buttonStyle
                    { onPress = Just <| Msg.OAuth <| Msg.SignIn
                    , label = El.text "Sign in"
                    }
                ]
        ]


renderPlayer : State -> Element Msg
renderPlayer state =
    if Dict.isEmpty state.videos then
        El.none
      else
        El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            [ El.el
                [ El.htmlAttribute <| HA.id playerId
                , El.width <| El.maximum 640 <| El.fill
                , El.height El.shrink
                , El.centerX
                ]
                El.none

            , case Dict.get state.current state.videos of
                Just listItem ->
                    El.column
                        []
                        [ El.text "Currently playing:"
                        , El.paragraph
                            [ Font.size 28
                            ]
                            [ El.text listItem.video.title
                            ]
                        , case ( listItem.video.startAt, listItem.video.endAt ) of
                            ( Just startAt, Just endAt ) ->
                                "({{}} - {{}})"
                                    |> String.Format.value (App.secondsToString <| Just startAt)
                                    |> String.Format.value (App.secondsToString <| Just endAt)
                                    |> El.text

                            ( Just startAt, Nothing ) ->
                                "({{}} - End)"
                                    |> String.Format.value (App.secondsToString <| Just startAt)
                                    |> El.text

                            ( Nothing, Just endAt ) ->
                                "(0:00 - {{}})"
                                    |> String.Format.value (App.secondsToString <| Just endAt)
                                    |> El.text

                            ( Nothing, Nothing ) ->
                                El.none
                        ]

                Nothing ->
                    El.none

            , case Dict.get (App.nextIndex state) state.videos of
                Just listItem ->
                    El.column
                        []
                        [ El.text "Up next:"
                        , El.paragraph
                            [ Font.size 20
                            ]
                            [ El.text listItem.video.title
                            ]
                        ]

                Nothing ->
                    El.none
            , El.row
                [ El.spacing 5
                ]
                [ Input.button
                    buttonStyle
                    { onPress = Just <| Msg.Player <| Msg.PlayPrevious
                    , label = El.text "Play previous"
                    }
                , Input.button
                    buttonStyle
                    { onPress = Just <| Msg.Player <| Msg.PlayNext
                    , label = El.text "Play next"
                    }
                ]
            ]


renderPlaylistList : State -> Element Msg
renderPlaylistList state =
    if Dict.isEmpty state.lists then
        El.none
    else
        let
            hasSelectedLists =
                not <| List.isEmpty (List.filter .checked (Dict.values state.lists))

            hasPlaylist =
                not <| Dict.isEmpty state.videos
        in
        El.column
            [ El.paddingXY 0 5
            , El.spacing 10
            , El.width El.fill
            ]
            [ El.el
                [ Font.size 24
                ]
                <| El.text "Select Playlists to mix"
            , El.row
                [ El.spacing 5
                ]
                <| case ( hasSelectedLists, hasPlaylist ) of
                    ( True, True ) ->
                        [ Input.button
                            buttonStyle
                            { onPress =
                                (Msg.PlaylistList << Msg.SetPlaylist)
                                    |> Msg.GetPlaylistVideos
                                    |> Msg.PlaylistList
                                    |> Just
                            , label = El.text "Confirm and replace current list"
                            }
                        , Input.button
                            buttonStyle
                            { onPress =
                                (Msg.PlaylistList << Msg.AppendPlaylist)
                                    |> Msg.GetPlaylistVideos
                                    |> Msg.PlaylistList
                                    |> Just
                            , label = El.text "Confirm and append to current list"
                            }
                        ]

                    ( False, True ) ->
                        [ El.el
                            (disabledButtonStyle state)
                            <| El.text "Confirm and replace current list"
                        , El.el
                            (disabledButtonStyle state)
                            <| El.text "Confirm and append to current list"
                        ]

                    ( True, False ) ->
                        [ Input.button
                            buttonStyle
                            { onPress =
                                (Msg.PlaylistList << Msg.SetPlaylist)
                                    |> Msg.GetPlaylistVideos
                                    |> Msg.PlaylistList
                                    |> Just
                            , label = El.text "Confirm"
                            }
                        ]

                    ( False, False ) ->
                        [ El.el
                            (disabledButtonStyle state)
                            <| El.text "Confirm"
                        ]
            , El.row
                [ El.spacing 5
                ]
                [ Input.button
                    buttonStyle
                    { onPress = Just <| Msg.PlaylistList <| Msg.SetCheckedAll
                    , label = El.text "Select all"
                    }
                , Input.button
                    buttonStyle
                    { onPress = Just <| Msg.PlaylistList <| Msg.SetCheckedNone
                    , label = El.text "Deselect all"
                    }
                ]
            , El.column
                [ El.spacing 5
                , El.padding 5
                , El.height
                    (El.shrink
                        |> El.maximum 400
                    )
                , El.width El.fill
                , El.scrollbarY
                ]
                <| List.map
                    (\listItem ->
                        El.row
                            [ El.spacing 20
                            ]
                            [ Input.checkbox
                                []
                                { onChange = Msg.PlaylistList << Msg.SetChecked listItem.playlist.id
                                , icon = Input.defaultCheckbox
                                , checked = listItem.checked
                                , label = Input.labelRight [] <| El.text listItem.playlist.title
                                }
                            , El.newTabLink
                                buttonStyle
                                { label = linkLabel "Open playlist"
                                , url = Playlist.url listItem.playlist
                                }
                            ]
                    )
                    (Dict.values state.lists
                        |> List.sortBy (.playlist >> .title)
                    )
            ]


renderVideoList : State -> Element Msg
renderVideoList state =
    if Dict.isEmpty state.videos then
        El.none
      else
        El.column
            [ El.scrollbarY
            , El.height <| El.maximum 600 El.shrink
            , El.htmlAttribute <| HA.id playlistId
            , El.width El.fill
            ]
            [ El.column
                [ El.spacing 15
                , El.padding 5
                ]
                <| List.map
                    (renderVideoListItem state)
                    (Dict.toList state.videos)
            ]


renderVideoListItem : State -> ( Int, App.VideoListItem ) -> Element Msg
renderVideoListItem state (index, listItem) =
    El.row
        [ El.spacing 5
        , El.htmlAttribute <| HA.id <| playlistVideoId index
        ]
        [ El.el
            [ El.alignTop ]
            <| El.text <| String.fromInt (index + 1) ++ "."
        , El.column
            [ El.width El.fill
            , El.spacing 5
            ]
            [ El.paragraph
                []
                [ El.text listItem.video.title
                ]
            , El.row
                [ El.spacing 10
                ]
                [ Input.button
                    buttonStyle
                    { onPress = Just <| Msg.VideoList <| Msg.PlayVideo index
                    , label = El.text "Play"
                    }
                , Input.button
                    buttonStyle
                    { onPress = Just (Msg.VideoList (Msg.ToggleEditVideo index (not listItem.editOpen)))
                    , label = El.text "Edit"
                    }
                , El.newTabLink
                    buttonStyle
                    { label = linkLabel "Open Video"
                    , url = Video.url listItem.video
                    }
                , El.newTabLink
                    buttonStyle
                    { label = linkLabel "Open Playlist"
                    , url = Playlist.url listItem.playlist
                    }
                ]
            , if listItem.editOpen then
                if Google.tokenHasWriteScope state.token then
                    El.column
                        [ El.spacing 10
                        ]
                        [ renderTimeInput
                            { error = listItem.startAtError
                            , label = "Start:"
                            , onChange = Msg.VideoList << Msg.SetVideoStartAt index
                            , onLoseFocus = Msg.VideoList <| Msg.ValidateVideoStartAt index
                            , value = listItem.startAt
                            }
                            state
                        , renderTimeInput
                            { error = listItem.endAtError
                            , label = "End:"
                            , onChange = Msg.VideoList << Msg.SetVideoEndAt index
                            , onLoseFocus = Msg.VideoList <| Msg.ValidateVideoEndAt index
                            , value = listItem.endAt
                            }
                            state
                        , Input.multiline
                            [ Background.color state.theme.bg
                            ]
                            { label = Input.labelLeft [] <| El.text "Note:"
                            , onChange = Msg.VideoList << Msg.SetVideoNote index
                            , placeholder = Nothing
                            , spellcheck = False
                            , text = listItem.note
                            }
                        , El.row
                            [ El.spacing 10
                            ]
                            [ Input.button
                                buttonStyle
                                { onPress = Just <| Msg.VideoList <| Msg.SaveVideoTimes index
                                , label = El.text "Save"
                                }
                            , Input.button
                                buttonStyle
                                { onPress = Just <| Msg.VideoList <| Msg.ToggleEditVideo index False
                                , label = El.text "Cancel"
                                }
                            ]
                        ]
                else
                    El.row
                        [ El.spacing 10
                        ]
                        [ El.text "Not signed in / not authorized"
                        , Input.button
                                buttonStyle
                                { onPress = Just <| Msg.OAuth <| Msg.SignIn
                                , label = El.text "Sign in"
                                }
                        ]
              else
                El.none
            ]
        ]


renderTimeInput :
    { error : Maybe String
    , label : String
    , onChange : String -> msg
    , onLoseFocus : msg
    , value : String
    }
    -> State
    -> Element msg
renderTimeInput data state =
    Input.text
        [ Background.color state.theme.bg
        , El.width <| El.px 75
        , El.below
            ( case data.error of
                Just error ->
                    El.el
                        [ Background.color state.theme.bg
                        , Border.color state.theme.error
                        , Border.width 1
                        , Border.rounded 5
                        , El.padding 5
                        ]
                        <| El.text error

                Nothing ->
                    El.none
            )
        , Events.onLoseFocus data.onLoseFocus
        , maybeAttribute data.error <| Border.color state.theme.error
        ]
        { label = Input.labelLeft [] <| El.text data.label
        , onChange = data.onChange
        , placeholder = Just <| Input.placeholder [] <| El.text "1:23"
        , text = data.value
        }


renderMessages : State -> Element msg
renderMessages state =
    if List.isEmpty state.messages then
        El.none
    else
        El.column
            [ El.padding 10
            , El.scrollbarY
            , El.height <| El.maximum 200 El.shrink
            ]
            <| List.map El.text state.messages


maybeAttribute : Maybe a -> El.Attribute msg -> El.Attribute msg
maybeAttribute maybe attr =
    if Maybe.Extra.isJust maybe then
        attr
    else
        El.focused []


buttonStyle : List (El.Attribute msg)
buttonStyle =
    [ El.padding 5
    , Border.width 1
    , Border.rounded 5
    ]


disabledButtonStyle : State -> List (El.Attribute msg)
disabledButtonStyle state =
    List.append
        buttonStyle
        [ Border.dashed
        , Border.color state.theme.disabled
        , Font.color state.theme.disabled
        ]


paddingZero : { top : Int, bottom : Int, left : Int, right : Int }
paddingZero =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


playerId : String
playerId =
    "player"


playlistId : String
playlistId =
    "playlist"


playlistVideoId : Int -> String
playlistVideoId index =
    "playlist-video-" ++ String.fromInt index
