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
            , if not <| Dict.isEmpty state.lists then
                renderPlaylistList state
              else
                El.none
            , El.column
                [ El.padding 10
                , El.scrollbarY
                ]
                <| List.map El.text state.messages
            , if not <| Dict.isEmpty state.videos then
                renderVideoList state
              else
                El.none
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
        []
        [ El.column
            [ El.spacing 5
            , El.padding 5
            ]
            <| List.map
                (\(idx, video) ->
                    El.text <| String.fromInt idx ++ ". " ++ video.title
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
