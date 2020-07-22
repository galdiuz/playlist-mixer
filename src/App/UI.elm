module App.UI exposing (..)

import Dict
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA

import App exposing (State)
import App.Msg as Msg exposing (Msg)
import Google


render : State -> Html Msg
render state =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        ]
        <| El.column
            [ El.width El.fill
            , El.height El.fill
            ]
            [ case state.token of
                Just _ ->
                    El.text "<Logged in>"
                Nothing ->
                    El.text "<Not logged in>"
            , El.el
                [ El.htmlAttribute <| HA.id playerId
                , El.width <| El.maximum 640 <| El.fill
                , El.height El.shrink
                ]
                El.none
            , if state.playlistInStorage then
                Input.button
                    []
                    { onPress = Just <| Msg.LoadListFromStorage
                    , label = El.text "Resume previous playlist"
                    }
              else
                El.none
            , Input.button
                []
                { onPress = Just Msg.SignIn
                , label = El.text "Sign in"
                }
            , case state.token of
                Just _ ->
                    Input.button
                        []
                        { onPress = Just <| Msg.GetUserPlaylists
                        , label = El.text "Get list"
                        }
                Nothing ->
                    El.none
            , if not <| Dict.isEmpty state.videos then
                renderVideoList state
              else
                El.none
            , if not <| Dict.isEmpty state.lists then
                renderPlaylistList state
              else
                El.none
            , El.column
                [ El.padding 10
                , El.scrollbarY
                ]
                <| List.map El.text state.messages
            ]


renderPlaylistList state =
    El.column
        [ El.padding 5
        ]
        [ El.row
            [ El.spacing 5
            ]
            [ Input.button
                buttonStyle
                { onPress =
                    state.lists
                        |> Dict.values
                        |> List.filterMap
                            (\listItem ->
                                if listItem.checked then
                                    Just listItem.item
                                else
                                    Nothing
                            )
                        |> Msg.GetAllVideos
                        |> Just
                , label = El.text "Confirm"
                }
            , Input.button
                buttonStyle
                { onPress = Just Msg.SetListAll
                , label = El.text "Select all"
                }
            , Input.button
                buttonStyle
                { onPress = Just Msg.SetListNone
                , label = El.text "Deselect all"
                }
            ]
        , El.column
            [ El.spacing 5
            , El.padding 5
            ]
            <| List.map
                (\list ->
                    Input.checkbox
                        []
                        { onChange = Msg.SetListChecked list.item.id
                        , icon = Input.defaultCheckbox
                        , checked = list.checked
                        , label = Input.labelRight [] <| El.text list.item.title
                        }
                )
                (Dict.values state.lists)
        ]


renderVideoList state =
    El.column
        [ El.scrollbarY
        , El.height <| El.minimum 600 <| El.shrink
        , El.htmlAttribute <| HA.id playlistId
        ]
        [ El.column
            [ El.spacing 10
            , El.padding 5
            ]
            <| List.map
                (\(index, listItem) ->
                    El.row
                        [ El.spacing 5
                        , El.htmlAttribute <| HA.id <| playlistVideoId index
                        ]
                        [ El.text <| String.fromInt (index + 1) ++ "."
                        , El.column
                            [ El.width El.fill
                            ]
                            [ El.paragraph [] [ El.text listItem.video.title ]
                            , El.row
                                [ El.spacing 10
                                ]
                                [ Input.button
                                    buttonStyle
                                    { onPress = Just <| Msg.PlayVideo index
                                    , label = El.text "Play"
                                    }
                                , Input.button
                                    buttonStyle
                                    { onPress = Just <| Msg.ToggleEditVideo index (not listItem.editOpen)
                                    , label = El.text "Edit"
                                    }
                                ]
                            , if listItem.editOpen then
                                if Google.tokenHasWriteScope state.token then
                                    El.row
                                        [ El.spacing 10
                                        ]
                                        [ renderTimeInput
                                            { error = listItem.startAtError
                                            , label = "Start:"
                                            , onChange = Msg.SetVideoStartAt index
                                            , onLoseFocus = Msg.ValidateVideoStartAt index
                                            , value = listItem.startAt
                                            }
                                        , renderTimeInput
                                            { error = listItem.endAtError
                                            , label = "End:"
                                            , onChange = Msg.SetVideoEndAt index
                                            , onLoseFocus = Msg.ValidateVideoEndAt index
                                            , value = listItem.endAt
                                            }
                                        , Input.button
                                            buttonStyle
                                            { onPress = Just <| Msg.SaveVideoTimes index
                                            , label = El.text "Save"
                                            }
                                        , Input.button
                                            buttonStyle
                                            { onPress = Just <| Msg.ToggleEditVideo index False
                                            , label = El.text "Cancel"
                                            }
                                        ]
                                else
                                    El.row
                                        [ El.spacing 10
                                        ]
                                        [ El.text "Not logged in / not authorized"
                                        , Input.button
                                                buttonStyle
                                                { onPress = Just Msg.SignIn
                                                , label = El.text "Sign in"
                                                }
                                        ]
                              else
                                El.none
                            ]
                        ]
                )
                (Dict.toList state.videos)
        ]


renderTimeInput :
    { error : Maybe String
    , label : String
    , onChange : String -> msg
    , onLoseFocus : msg
    , value : String
    }
    -> Element msg
renderTimeInput data =
    Input.text
        [ El.width <| El.px 75
        , El.below
            ( case data.error of
                Just error ->
                    El.el
                        [ Background.color <| El.rgb 1 1 1
                        , Border.color <| El.rgb 1 0 0
                        , Border.width 1
                        , Border.rounded 5
                        , El.padding 5
                        ]
                        <| El.text error

                Nothing ->
                    El.none
            )
        , Events.onLoseFocus data.onLoseFocus
        , maybeAttribute data.error <| Border.color <| El.rgb 1 0 0
        ]
        { label = Input.labelLeft [] <| El.text data.label
        , onChange = data.onChange
        , placeholder = Just <| Input.placeholder [] <| El.text "1:23"
        , text = data.value
        }


maybeAttribute : Maybe a -> El.Attribute msg -> El.Attribute msg
maybeAttribute maybe attr =
    case maybe of
        Just _ ->
            attr

        Nothing ->
            El.focused []


buttonStyle : List (El.Attribute msg)
buttonStyle =
    [ El.padding 5
    , Border.width 1
    , Border.rounded 5
    ]


playerId : String
playerId =
    "player"


playlistId : String
playlistId =
    "playlist"


playlistVideoId : Int -> String
playlistVideoId index =
    "playlist-video-" ++ String.fromInt index
