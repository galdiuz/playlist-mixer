module App.UI exposing
    ( playerId
    , render
    , videoListId
    , videoListVideoId
    )

import Array
import Array.Extra
import Dict
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Maybe.Extra
import OAuth
import OAuth.Implicit as OAuth
import String.Format
import Url.Builder

import App exposing (State)
import App.Msg as Msg exposing (Msg)
import Google.OAuth
import Google.OAuth.Scope
import Youtube.PlayerError exposing (PlayerError)
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
                        , renderConfig state
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
        [ El.paddingEach { paddingZero | top = 15 }
        , El.spacing 10
        , El.width El.fill
        ]
        [ El.row
            [ El.width El.fill
            ]
            [ El.el
                [ Font.bold
                , Font.size 32
                ]
                <| El.text "Playlist Mixer"
            , renderDevelopedWithYoutube state
            ]
        , renderSpacer state
        ]


renderFooter : State -> Element Msg
renderFooter state =
    El.column
        [ El.width El.fill
        , El.paddingEach { paddingZero | bottom = 15 }
        , El.spacing 20
        ]
        [ renderSpacer state
        , El.row
            [ El.spacing 10
            ]
            [ El.newTabLink
                buttonStyle
                { label = renderLinkLabel "View privacy policy"
                , url = App.privacyPolicyUrl state
                }
            , El.newTabLink
                buttonStyle
                { label = renderLinkLabel "View source on GitHub"
                , url = "https://github.com/galdiuz/playlist-mixer"
                }
            ]
        ]


renderLinkLabel : String -> Element msg
renderLinkLabel text =
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
    if Array.isEmpty state.videoList then
        El.paragraph
            []
            [ El.text
                <| "Welcome to Playlist Mixer! This application lets you mix multiple YouTube"
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
        [ renderPlaylistMenuResume state
        , if Maybe.Extra.isJust state.token then
            El.column
                [ El.spacing 30
                , El.width El.fill
                ]
                [ renderPlaylistMenuLoadFromAccount state
                , renderPlaylistMenuLoadByUrl state
                , renderPlaylistMenuLoadByChannel state
                ]

          else
            renderPlaylistMenuSignIn state
        ]


renderPlaylistMenuResume : State -> Element Msg
renderPlaylistMenuResume state =
    if state.playlistInStorage && Array.isEmpty state.videoList then
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


renderPlaylistMenuLoadFromAccount : State -> Element Msg
renderPlaylistMenuLoadFromAccount state =
    El.column
        [ El.spacing 10
        ]
        [ El.el
            [ Font.size 24
            ]
            <| El.text "Load playlists from your YouTube account"
        , if Google.OAuth.tokenHasReadScope state.token then
            Input.button
                buttonStyle
                { onPress = Just <| Msg.PlaylistList <| Msg.GetUserPlaylists
                , label = El.text "Load"
                }
          else
            El.column
                [ El.spacing 10
                ]
                [ El.paragraph
                    []
                    [ El.text
                        <| "To fetch playlists from your YouTube account Playlist Mixer needs"
                        ++ " permission to view your YouTube account. Any data fetched from your"
                        ++ " account will only be stored locally in your browser. For more"
                        ++ " information, refer to "
                    , El.newTabLink
                        [ Font.underline
                        ]
                        { label = El.text "Playlist Mixer's privacy policy"
                        , url = App.privacyPolicyUrl state
                        }
                    , El.text "."
                    ]
                , Input.button
                    []
                    { onPress =
                        [ Google.OAuth.Scope.YoutubeReadOnly
                        ]
                            |> Msg.SignIn
                            |> Msg.OAuth
                            |> Just
                    , label = renderSignInButton
                    }
                ]
        ]


renderPlaylistMenuLoadByUrl : State -> Element Msg
renderPlaylistMenuLoadByUrl state =
    El.column
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
            , text = state.playlistsByUrlValue
            }
        , if String.isEmpty state.playlistsByUrlValue then
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


renderPlaylistMenuLoadByChannel : State -> Element Msg
renderPlaylistMenuLoadByChannel state =
    El.column
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
            , text = state.playlistsByChannelValue
            }
        , if String.isEmpty state.playlistsByChannelValue then
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


renderPlaylistMenuSignIn : State -> Element Msg
renderPlaylistMenuSignIn state =
    El.column
        [ El.spacing 10
        ]
        [ El.paragraph
            []
            [ El.text
                <| "Fetching playlists from YouTube's APIs requires you to be signed in to"

                ++ " a Google  account. Playlist Mixer does not collect any personal data."
                ++ " For more information, refer to "
            , El.newTabLink
                [ Font.underline
                ]
                { label = El.text "Playlist Mixer's privacy policy"
                , url = App.privacyPolicyUrl state
                }
            , El.text "."
            ]
        , Input.button
            []
            { onPress = Just <| Msg.OAuth <| Msg.SignIn []
            , label = renderSignInButton
            }
        ]


renderPlayer : State -> Element Msg
renderPlayer state =
    if Array.isEmpty state.videoList then
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
            , case App.getVideoItem state state.currentVideoIndex of
                Just { listItem, video } ->
                    El.column
                        []
                        [ El.text "Currently playing:"
                        , El.paragraph
                            [ Font.size 28
                            ]
                            [ El.text video.title
                            ]
                        , renderTimeRange listItem
                        ]

                Nothing ->
                    El.none
            , case App.getVideoItem state (App.nextIndex state) of
                Just { video } ->
                    El.column
                        []
                        [ El.text "Up next:"
                        , El.paragraph
                            [ Font.size 20
                            ]
                            [ El.text video.title
                            ]
                        ]

                Nothing ->
                    El.none
            , El.row
                [ El.spacing 10
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


renderTimeRange : App.VideoListItem -> Element msg
renderTimeRange listItem =
    El.none
    -- case ( listItem.video.startAt, listItem.video.endAt ) of
    --     ( Just startAt, Just endAt ) ->
    --         "({{}} - {{}})"
    --             |> String.Format.value (App.secondsToString <| Just startAt)
    --             |> String.Format.value (App.secondsToString <| Just endAt)
    --             |> El.text

    --     ( Just startAt, Nothing ) ->
    --         "({{}} - End)"
    --             |> String.Format.value (App.secondsToString <| Just startAt)
    --             |> El.text

    --     ( Nothing, Just endAt ) ->
    --         "(0:00 - {{}})"
    --             |> String.Format.value (App.secondsToString <| Just endAt)
    --             |> El.text

    --     ( Nothing, Nothing ) ->
    --         El.none


renderPlaylistList : State -> Element Msg
renderPlaylistList state =
    if Dict.isEmpty state.playlistList then
        El.none
    else
        let
            hasSelectedLists =
                not <| List.isEmpty (List.filter .checked (Dict.values state.playlistList))

            hasPlaylist =
                not <| Array.isEmpty state.videoList
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
                , El.height <| El.maximum 400 El.shrink
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
                                { label = renderLinkLabel "Open playlist"
                                , url = Playlist.url listItem.playlist
                                }
                            ]
                    )
                    (Dict.values state.playlistList
                        |> List.sortBy (.playlist >> .title)
                    )
            ]


renderVideoList : State -> Element Msg
renderVideoList state =
    if Array.isEmpty state.videoList then
        El.none
      else
        let
            isSearching =
                not <| String.isEmpty state.searchValue

            videoList =
                if isSearching then
                    Array.filter
                        (\listItem ->
                            state.searchValue
                                |> String.words
                                |> List.all
                                    (\word ->
                                        String.contains
                                            (String.toLower word)
                                            (Dict.get listItem.videoId
                                                |> Maybe.map .title
                                                |> Maybe.map String.toLower
                                                |> Maybe.withDefault ""
                                            )
                                    )
                        )
                        state.videoList
                        |> Array.indexedMapToList Tuple.pair
                else
                    Array.slice
                        state.currentListIndex
                        (state.currentListIndex + App.videoListPageSize)
                        state.videoList
                        |> Array.Extra.indexedMapToList
                            (\index listItem ->
                                ( index + state.currentListIndex, listItem )
                            )
        in
        El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            [ El.row
                [ El.spacing 10
                ]
                [ if not isSearching then
                    "Showing {{}}-{{}} (of {{}})"
                        |> String.Format.value (String.fromInt <| state.currentListIndex + 1)
                        |> String.Format.value
                            (String.fromInt
                                (min
                                    (Array.length state.videoList)
                                    (state.currentListIndex + App.videoListPageSize)
                                )
                            )
                        |> String.Format.value (String.fromInt <| Array.length state.videoList)
                        |> El.text
                  else if Array.length videoList > App.videoListPageSize then
                    "Showing first {{}} matches"
                        |> String.Format.value (String.fromInt App.videoListPageSize)
                        |> El.text
                  else
                    "Showing {{}} matches"
                        |> String.Format.value (String.fromInt <| Array.length videoList)
                        |> El.text
                , Input.text
                    [ Background.color state.theme.bg
                    ]
                    { label = Input.labelLeft [] <| El.text "Search:"
                    , onChange = Msg.VideoList << Msg.SetSearch
                    , placeholder = Nothing
                    , text = state.searchValue
                    }
                ]
            , if isSearching then
                El.none
              else
                El.row
                    [ El.spacing 10
                    ]
                    [ Input.button
                        buttonStyle
                        { label = El.text "Show current video"
                        , onPress = Just <| Msg.VideoList Msg.ShowCurrentVideo
                        }
                    ]
            , El.column
                [ El.scrollbarY
                , El.height <| El.maximum 600 El.shrink
                , El.htmlAttribute <| HA.id videoListId
                , El.width El.fill
                , El.htmlAttribute
                    (Html.Events.stopPropagationOn
                        "scroll"
                        (Decode.map
                            (\scroll ->
                                ( Msg.VideoList <| Msg.Scroll scroll
                                , True
                                )
                            )
                            App.decodeScrollPos
                        )
                    )
                ]
                [ El.column
                    [ El.spacing 10
                    , El.padding 5
                    , El.height El.shrink
                    ]
                    (videoList
                        |> List.take App.videoListPageSize
                        |> List.map
                            (\( index, listItem ) ->
                                Keyed.el
                                    []
                                    ( listItem.videoId ++ String.fromInt listItem.segmentIndex
                                    , Lazy.lazy (renderVideoListItem state index) listItem
                                    )
                            )
                        |> List.intersperse (renderSpacer state)
                    )
                ]
            ]


renderVideoListItem : State -> Int -> App.VideoListItem -> Element Msg
renderVideoListItem state index listItem =
    El.row
        [ El.spacing 5
        , El.htmlAttribute <| HA.id <| videoListVideoId index
        , El.width El.fill
        ]
        [ El.el
            [ El.alignTop
            , maybeAttribute (index == state.currentVideoIndex) Font.bold
            ]
            <| El.text <| String.fromInt (index + 1) ++ "."
        , El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            [ El.paragraph
                [ maybeAttribute (index == state.currentVideoIndex) Font.bold
                ]
                [ El.text <| Maybe.withDefault "" <| Dict.get listItem.videoId
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
                    { onPress = Just <| Msg.VideoList <| Msg.ToggleEditVideo index <| not listItem.editOpen
                    , label = El.text "Edit"
                    }
                , El.newTabLink
                    buttonStyle
                    { label = renderLinkLabel "Open Video"
                    , url = Video.url listItem.video
                    }
                , El.newTabLink
                    buttonStyle
                    { label = renderLinkLabel "Open Playlist"
                    , url = Playlist.url listItem.playlist
                    }
                ]
            , if listItem.editOpen then
                renderVideoListItemEdit state index listItem
              else
                El.none
            , case listItem.error of
                Just error ->
                    El.paragraph
                        [ Border.width 1
                        , Border.color <| state.theme.error
                        , El.padding 2
                        ]
                        [ El.text "Error playing video: "
                        , El.text <| Youtube.PlayerError.description error
                        ]
                Nothing ->
                    El.none
            ]
        ]


renderVideoListItemEdit : State -> Int -> App.VideoListItem -> Element Msg
renderVideoListItemEdit state index listItem =
    if Google.OAuth.tokenHasWriteScope state.token then
        El.column
            [ El.spacing 10
            ]
            []
            -- [ renderTimeInput
            --     { error = listItem.startAtError
            --     , label = "Start:"
            --     , onChange = Msg.VideoList << Msg.SetVideoStartAt index
            --     , onLoseFocus = Msg.VideoList <| Msg.ValidateVideoStartAt index
            --     , value = listItem.startAtValue
            --     }
            --     state
            -- , renderTimeInput
            --     { error = listItem.endAtError
            --     , label = "End:"
            --     , onChange = Msg.VideoList << Msg.SetVideoEndAt index
            --     , onLoseFocus = Msg.VideoList <| Msg.ValidateVideoEndAt index
            --     , value = listItem.endAtValue
            --     }
            --     state
            -- , Input.multiline
            --     [ Background.color state.theme.bg
            --     ]
            --     { label = Input.labelLeft [] <| El.text "Note:"
            --     , onChange = Msg.VideoList << Msg.SetVideoNote index
            --     , placeholder = Nothing
            --     , spellcheck = False
            --     , text = listItem.note
            --     }
            -- , El.row
            --     [ El.spacing 10
            --     ]
            --     [ Input.button
            --         buttonStyle
            --         { onPress = Just <| Msg.VideoList <| Msg.SaveVideoTimes index
            --         , label = El.text "Save"
            --         }
            --     , Input.button
            --         buttonStyle
            --         { onPress = Just <| Msg.VideoList <| Msg.ToggleEditVideo index False
            --         , label = El.text "Cancel"
            --         }
            --     ]
            -- ]
    else
        El.column
            [ El.spacing 10
            ]
            [ El.paragraph
                []
                [ El.text
                    <| "To save notes on your playlist Playlist Mixer needs permission to manage"
                    ++ " your YouTube account. Playlist Mixer will only use this permission for"
                    ++ " this purpose and nothing else. For more information, refer to "
                , El.newTabLink
                    [ Font.underline
                    ]
                    { label = El.text "Playlist Mixer's privacy policy"
                    , url = App.privacyPolicyUrl state
                    }
                , El.text "."
                ]
            , Input.button
                []
                { onPress =
                    [ Google.OAuth.Scope.Youtube
                    ]
                        |> Msg.SignIn
                        |> Msg.OAuth
                        |> Just
                , label = renderSignInButton
                }
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
        , maybeAttribute (Maybe.Extra.isJust data.error) (Border.color state.theme.error)
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


renderConfig : State -> Element Msg
renderConfig state =
    El.column
        [ El.width El.fill
        , El.spacing 10
        ]
        [ renderSpacer state
        , El.el
            [ Font.size 24
            , El.paddingEach { paddingZero | top = 10 }
            ]
            <| El.text "Configuration"
        , El.row
            [ El.spacing 10
            ]
            [ Input.button
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
        , Input.checkbox
            []
            { checked = state.autoplay
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] <| El.text "Automatically play next video"
            , onChange = Msg.SetAutoplay
            }
        , case Maybe.andThen .email state.token of
            Just email ->
                El.row
                    [ El.spacing 5
                    ]
                    [ El.paragraph
                        []
                        [ El.text <| "Signed in as " ++ email
                        ]
                    , Input.button
                        buttonStyle
                        { onPress = Just <| Msg.OAuth <| Msg.SignOut
                        , label = El.text "Sign out"
                        }
                    ]
            Nothing ->
                El.none
        ]


maybeAttribute : Bool -> El.Attribute msg -> El.Attribute msg
maybeAttribute bool attr =
    if bool then
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


renderSignInButton : Element msg
renderSignInButton =
    El.image
        []
        { description = "Sign in with Google"
        , src =
            Url.Builder.relative
                [ "media/btn_google_signin_dark_normal_web.png" ]
                []
        }


renderDevelopedWithYoutube : State -> Element msg
renderDevelopedWithYoutube state =
    let
        mask =
            "url(media/developed-with-youtube.svg)"
    in
    El.el
        [ Background.color <| state.theme.fg
        , El.alignRight
        , El.height <| El.minimum 30 El.shrink
        , El.htmlAttribute <| HA.style "-webkit-mask" mask
        , El.htmlAttribute <| HA.style "-webkit-mask-size" "contain"
        , El.htmlAttribute <| HA.style "mask" mask
        , El.htmlAttribute <| HA.style "mask-size" "contain"
        , El.width <| El.minimum 243 El.shrink
        ]
        El.none


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


videoListId : String
videoListId =
    "video-list"


videoListVideoId : Int -> String
videoListVideoId index =
    "video-list-video-" ++ String.fromInt index
