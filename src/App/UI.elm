module App.UI exposing (..)

import Dict
import Element as El exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA

import App exposing (State)
import App.Msg as Msg exposing (Msg)


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
            [ El.el
                [ El.htmlAttribute <| HA.id playerId
                ]
                El.none
            , El.text "Resume previous playlist"
            , Input.button
                []
                { onPress = Just Msg.SignIn
                , label = El.text "Sign in"
                }
            , Input.button
                []
                { onPress = Just <| Msg.PlayList
                , label = El.text "Load from storage"
                }
            , Input.button
                []
                { onPress = Just <| Msg.GetUserPlaylists
                , label = El.text "Get list"
                }
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
        ]
        [ El.column
            [ El.spacing 10
            , El.padding 5
            ]
            <| List.map
                (\(index, listItem) ->
                    El.row
                        [ El.spacing 5
                        ]
                        [ El.text <| String.fromInt index ++ "."
                        , El.column
                            [ El.width El.fill
                            ]
                            [ El.paragraph [] [ El.text listItem.video.title ]
                            , El.row
                                [ El.spacing 5
                                ]
                                [ Input.button
                                    []
                                    { onPress = Nothing
                                    , label = El.text "Play"
                                    }
                                , El.text "-"
                                , Input.button
                                    []
                                    { onPress = Just <| Msg.ToggleEditVideo index True
                                    , label = El.text "Edit"
                                    }
                                ]
                            , if listItem.editOpen then
                                El.row
                                    [ El.spacing 10
                                    ]
                                    [ El.row
                                        [ El.spacing 2
                                        ]
                                        [ Input.text
                                            [ El.width <| El.px 75
                                            ]
                                            { label = Input.labelLeft [] <| El.text "Start:"
                                            , onChange = Msg.SetVideoStartAt index
                                            , placeholder = Just <| Input.placeholder [] <| El.text "123"
                                            , text =
                                                case listItem.startAt of
                                                    Just startAt ->
                                                        String.fromInt startAt
                                                    Nothing ->
                                                        ""
                                            }
                                        , El.text "s"
                                        ]
                                    , El.row
                                        [ El.spacing 2
                                        ]
                                        [ Input.text
                                            [ El.width <| El.px 75
                                            ]
                                            { label = Input.labelLeft [] <| El.text "End:"
                                            , onChange = Msg.SetVideoEndAt index
                                            , placeholder = Just <| Input.placeholder [] <| El.text "123"
                                            , text =
                                                case listItem.endAt of
                                                    Just endAt ->
                                                        String.fromInt endAt
                                                    Nothing ->
                                                        ""
                                            }
                                        , El.text "s"
                                        ]
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
                                El.none
                            ]
                        ]
                )
                (Dict.toList state.videos)
        ]


buttonStyle : List (El.Attribute msg)
buttonStyle =
    [ El.padding 5
    , Border.width 1
    , Border.rounded 5
    ]


playerId : String
playerId =
    "player"
